module Utils (
  pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount,
  phasOneScriptInputAtValidatorWithExactlyOneToken,
  phasOnlyOneValidScriptOutputWithToken,
  phasOnlyOneInputWithExactlyOneTokenWithSymbol,
  phasNoScriptInputWithToken,
  poutputsDoNotContainToken,
  pemptyTokenName,
  pands,
)
where

import Cardano.YTxP.Control.YieldList (PYieldListDatum (PYieldListDatum))
import Data.List.NonEmpty (nonEmpty)
import Numeric.Natural (Natural)
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value (
  PCurrencySymbol,
  PTokenName,
  PValue,
  padaSymbol,
  pvalueOf,
 )
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  POutputDatum (POutputDatum),
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Extra.Map (pkeys)
import Plutarch.List (pfoldl')

{- | Like Haskell's `and` but for Plutarch terms
`Plutarch.Bool` has the same function but does not export it.
-}
pands :: [Term s PBool] -> Term s PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts

{- | Check that none of the given outputs contain a token
with the given `PCurrencySymbol`
-}
poutputsDoNotContainToken ::
  Term s (PBuiltinList PTxOut) ->
  Term s (PCurrencySymbol :--> PBool)
poutputsDoNotContainToken outputs =
  plam $ \symbol ->
    pmatch
      ( pfilter
          # ( plam $ \txOut ->
                ( pconstant 0
                    #< (
                         -- We inline the `psymbolValue` function for efficiency reasons
                         let valueMap =
                              pto $
                                pto $
                                  pfromData $
                                    pfield @"value"
                                      # txOut
                             go = pfix #$ plam $ \self valueMap' ->
                              pelimList
                                ( \symbolAndTokens rest ->
                                    pif
                                      (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                      ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                         in pfoldl'
                                              ( \acc tokenAndAmount ->
                                                  (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                              )
                                              # 0
                                              # tokens
                                      )
                                      (self # rest)
                                )
                                0
                                valueMap'
                          in go # valueMap
                       )
                )
            )
          # outputs
      )
      $ \case
        -- Check that the filtered list is empty
        PNil -> pconstant True
        _ -> pconstant False

-- Check that none of the inputs in the given list
-- contain one or more tokens with the given `PCurrencySymbol`
phasNoScriptInputWithToken ::
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PCurrencySymbol :--> PBool)
phasNoScriptInputWithToken inputs =
  plam $ \symbol ->
    pmatch
      ( pfilter
          # ( plam $ \txInInfo ->
                pmatch
                  ( pfromData
                      $ pfield @"credential"
                        #$ pfromData
                      $ pfield @"address"
                        #$ pfromData
                      $ pfield @"resolved" # txInInfo
                  )
                  $ \case
                    -- Check the output is a script output,
                    -- if so check that the output contains one or more YieldListSTT
                    PScriptCredential _ ->
                      ( pconstant 0
                          #< (
                               -- We inline the `psymbolValue` function for efficiency reasons
                               let valueMap =
                                    pto
                                      $ pto
                                      $ pfromData
                                      $ pfield @"value"
                                        #$ pfromData
                                      $ pfield @"resolved"
                                        # txInInfo
                                   go = pfix #$ plam $ \self valueMap' ->
                                    pelimList
                                      ( \symbolAndTokens rest ->
                                          pif
                                            (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                            ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                               in pfoldl'
                                                    ( \acc tokenAndAmount ->
                                                        (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                                    )
                                                    # 0
                                                    # tokens
                                            )
                                            (self # rest)
                                      )
                                      0
                                      valueMap'
                                in go # valueMap
                             )
                      )
                    _ -> pconstant False
            )
          # inputs
      )
      $ \case
        PNil -> pconstant True
        _ -> pconstant False

{- | Check that there is only token of given `PCurrencySymbol`
 and `PTokenName` with given amount contained in the given PValue.

 Note: We use this function when checking the `mint` field of `TxInfo`,
 as when checking `mint` we need to check that the length is equal to two
 to account for the additional 'zero Ada' entry automatically included in the `mint` field:
   (PCurrencySymbol 0x,PMap [(PTokenName 0x,0)])])
-}
pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue keys amounts
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PBool
    )
pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount = phoistAcyclic $
  plam $ \value symbol tokenName amount ->
    pvalueOf
      # value
      # symbol
      # tokenName
      #== amount
      #&& (plength #$ pto $ pto $ pto value)
      #== 2

{- |
Check that:
  - there is exactly one script output with exactly one token
    with the given `PCurrencySymbol` and `PTokenName`
  - there is no other script output with one or more of these tokens
    (We do this in order to use this helper to ensure that there are no other
     script outputs containing one ore more of this token)
  - there is no wallet output that contains one of these tokens
  - if there is exactly one script output with exactly one of these tokens,
    we check that it contains no other tokens aside from Ada
  - finally check that the output also contains a valid (according to the spec) `POutputDatum`,
    ensuring the length of the yield list does not exceed the provided `maxYieldListSize` parameter.
-}
phasOnlyOneValidScriptOutputWithToken ::
  Natural ->
  Term s (PBuiltinList PTxOut) ->
  Term
    s
    ( PCurrencySymbol
        :--> PBool
    )
phasOnlyOneValidScriptOutputWithToken maxYieldListSize outputs =
  plam $ \symbol ->
    pmatch
      ( pfilter
          # ( plam $ \txOut ->
                -- Check the output contains one or more YieldListSTT.
                -- At this step, we want to find any output (wallet or script)
                -- that contains one or more YieldListSTT.
                -- This is to avoid needing another helper to check that there are no script outputs or
                -- wallet outputs containing one or more of these tokens.
                ( pconstant 0
                    #< (
                         -- We inline the `psymbolValue` function for efficiency reasons
                         let valueMap = pto (pto $ pfromData $ pfield @"value" # txOut)
                             go = pfix #$ plam $ \self valueMap' ->
                              pelimList
                                ( \symbolAndTokens rest ->
                                    pif
                                      (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                      ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                         in pfoldl'
                                              ( \acc tokenAndAmount ->
                                                  (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                              )
                                              # 0
                                              # tokens
                                      )
                                      (self # rest)
                                )
                                0
                                valueMap'
                          in go # valueMap
                       )
                )
            )
          # outputs
      )
      $ \case
        -- Check that the resulting filtered list has exactly one element
        PCons txOut' tailOfList ->
          pmatch (pnull # tailOfList) $ \case
            PTrue ->
              -- If so then check that the output is a script output
              pmatch
                (pfromData $ pfield @"credential" #$ pfromData $ pfield @"address" # txOut')
                $ \case
                  PScriptCredential _ ->
                    -- If there is only one output in the filtered result,
                    -- and that output is a script output,
                    -- then check that it contains exactly one token with the given symbol
                    pmatch
                      ( ( -- We inline the `psymbolValue` function for efficiency reasons
                          let valueMap = pto (pto $ pfromData $ pfield @"value" # txOut')
                              go = pfix #$ plam $ \self valueMap' ->
                                pelimList
                                  ( \symbolAndTokens rest ->
                                      pif
                                        (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                        ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                           in pfoldl'
                                                ( \acc tokenAndAmount ->
                                                    (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                                )
                                                # 0
                                                # tokens
                                        )
                                        (self # rest)
                                  )
                                  0
                                  valueMap'
                           in go # valueMap
                        )
                          #== pconstant 1
                      )
                      $ \case
                        -- If it contains exactly one token with the given symbol
                        -- then check that it does not contain any other tokens
                        -- aside from Ada and the given token
                        PTrue -> pmatch
                          ( ( pfilter
                                # ( plam $ \symbolInValue ->
                                      ( pnot
                                          #$ (pfromData symbolInValue)
                                          #== symbol
                                          #|| (pfromData symbolInValue)
                                          #== padaSymbol
                                      )
                                  )
                                # (pkeys #$ pto $ pfromData $ pfield @"value" # txOut')
                            )
                          )
                          $ \case
                            -- Finally check that it holds a valid output datum
                            -- With the length of the yield list not exceeding `maxYieldListSize` parameter
                            PNil ->
                              pmatch (pfromData $ pfield @"datum" # txOut') $ \case
                                POutputDatum (pfromData . (pfield @"outputDatum" #) -> datum) -> unTermCont $ do
                                  PYieldListDatum ((pfield @"yieldedToScripts" #) -> yieldedToList) <-
                                    pmatchC $ pfromData $ flip ptryFrom fst $ pto datum
                                  pure $
                                    plength # (pfromData yieldedToList) #<= pconstant (toInteger maxYieldListSize)
                                _ -> pconstant False
                            _ -> pconstant False
                        PFalse -> pconstant False
                  PPubKeyCredential _ -> pconstant False
            PFalse -> pconstant False
        PNil -> pconstant False

-- Check that there is one input with exactly one token of given `PCurrencySymbol`,
-- this input sits at a script address matching the given `PTxOutRef`,
-- and there are no other script inputs with one or more of the token with the given symbol.
phasOneScriptInputAtValidatorWithExactlyOneToken ::
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PCurrencySymbol :--> PTxOutRef :--> PBool)
phasOneScriptInputAtValidatorWithExactlyOneToken inputs =
  plam $ \symbol txOutRef ->
    pmatch
      ( pfilter
          # ( plam $ \txInInfo ->
                pmatch
                  ( pfromData
                      $ pfield @"credential"
                        #$ pfromData
                      $ pfield @"address"
                        #$ pfromData
                      $ pfield @"resolved"
                        # txInInfo
                  )
                  $ \case
                    -- We're only interested in script inputs
                    PScriptCredential _ ->
                      -- Check the inputs contains one or more YieldListSTT.
                      -- At this step, we want to find any input that contains one or more YieldListSTT.
                      -- This is to avoid needing another helper to check that there are no other script
                      -- inputs containing one or more of these tokens.
                      ( pconstant 0
                          #< (
                               -- We inline the `psymbolValue` function for efficiency reasons
                               let valueMap =
                                    pto
                                      $ pto
                                      $ pfromData
                                      $ pfield @"value"
                                        #$ pfromData
                                      $ pfield @"resolved"
                                        # txInInfo
                                   go = pfix #$ plam $ \self valueMap' ->
                                    pelimList
                                      ( \symbolAndTokens rest ->
                                          pif
                                            (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                            ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                               in pfoldl'
                                                    ( \acc tokenAndAmount ->
                                                        (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                                    )
                                                    # 0
                                                    # tokens
                                            )
                                            (self # rest)
                                      )
                                      0
                                      valueMap'
                                in go # valueMap
                             )
                      )
                    PPubKeyCredential _ -> pconstant False
            )
          # inputs
      )
      $ \case
        PCons txInInfo' tailOfList ->
          -- Check there's exactly one element in the filtered list
          pmatch (pnull # tailOfList) $ \case
            PTrue ->
              -- Check that the given ref and the ref of this input match
              pmatch (txOutRef #== (pfromData $ pfield @"outRef" # txInInfo')) $ \case
                PTrue ->
                  -- Check that the input contains exactly one token with the given symbol
                  ( ( -- We inline the `psymbolValue` function for efficiency reasons
                      let valueMap =
                            pto
                              $ pto
                              $ pfromData
                              $ pfield @"value"
                                #$ pfromData
                              $ pfield @"resolved"
                                # txInInfo'
                          go = pfix #$ plam $ \self valueMap' ->
                            pelimList
                              ( \symbolAndTokens rest ->
                                  pif
                                    (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                    ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                       in pfoldl'
                                            ( \acc tokenAndAmount ->
                                                (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                            )
                                            # 0
                                            # tokens
                                    )
                                    (self # rest)
                              )
                              0
                              valueMap'
                       in go # valueMap
                    )
                      #== pconstant 1
                  )
                PFalse -> pconstant False
            PFalse -> pconstant False
        _ -> pconstant False

{- | Check that there is exactly one input,
and that one input carries exactly one yield list token.
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbol ::
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PCurrencySymbol :--> PBool)
phasOnlyOneInputWithExactlyOneTokenWithSymbol inputs =
  plam $ \symbol ->
    pmatch inputs $
      \case
        PCons txInInfo tailOfList ->
          pmatch (pnull # tailOfList) $ \case
            PTrue ->
              ( ( -- We inline the `psymbolValue` function for efficiency reasons
                  let valueMap =
                        pto
                          $ pto
                          $ pfromData
                          $ pfield @"value"
                            #$ pfromData
                          $ pfield @"resolved"
                            # txInInfo
                      go = pfix #$ plam $ \self valueMap' ->
                        pelimList
                          ( \symbolAndTokens rest ->
                              pif
                                (pfromData (pfstBuiltin # symbolAndTokens) #== symbol)
                                ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                                   in pfoldl'
                                        ( \acc tokenAndAmount ->
                                            (pfromData $ psndBuiltin # tokenAndAmount) + acc
                                        )
                                        # 0
                                        # tokens
                                )
                                (self # rest)
                          )
                          0
                          valueMap'
                   in go # valueMap
                )
                  #== pconstant 1
              )
            _ -> pconstant False
        _ -> pconstant False

-- | Empty token name
pemptyTokenName :: Term s PTokenName
pemptyTokenName = pconstant ""
