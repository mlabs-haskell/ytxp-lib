{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Cardano.YTxP.Control.YieldList.MintingPolicy (
  -- * Script
  YieldListSTMPScript (..),
  compileYieldListSTMP,

  -- * Currency Symbol
  YieldListSTCS,
  mkYieldListSTCS,

  -- * Helpers
  pcontainsYieldListSTT,
) where

import Cardano.YTxP.Control.YieldList (
  PYieldListMPWrapperRedeemer (PBurn, PMint),
 )
import Control.Monad (void, (<=<))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON, withText)
import Data.String (IsString, fromString)
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V1.Value (PValue)
import Plutarch.Api.V2 (PScriptContext, PScriptPurpose (PMinting), scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)
import Prettyprinter (Pretty)
import Utils (
  pands,
  pemptyTokenName,
  phasNoScriptInputWithToken,
  phasOnlyOneInputWithExactlyOneTokenWithSymbol,
  phasOnlyOneValidScriptOutputWithToken,
  pmember,
  pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount,
 )

--------------------------------------------------------------------------------
-- YieldListSTMPScript

-- | @since 0.1.0
newtype YieldListSTMPScript = YieldListSTMPScript Script
  deriving
    ( -- | @since 0.1.0
      ToJSON
    , -- | @since 0.1.0
      FromJSON
    )
    via (HexStringScript "YieldListSTMPScript")
  deriving
    ( --- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Pretty
    )
    via SerialForm

compileYieldListSTMP ::
  -- | Plutarch compilation configuration
  Config ->
  Natural ->
  (forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque)) ->
  Either Text YieldListSTMPScript
compileYieldListSTMP config maxYieldListSize scriptToWrap = do
  let
    yieldListSTMPWrapper ::
      ( Term
          s
          ( (PData :--> PScriptContext :--> POpaque)
              :--> PData
              :--> PScriptContext
              :--> POpaque
          )
      )
    yieldListSTMPWrapper = mkYieldListSTMPWrapper maxYieldListSize

  script <- compile config (yieldListSTMPWrapper # scriptToWrap)

  pure $ YieldListSTMPScript script

--------------------------------------------------------------------------------
-- YieldListSTCS
-- TODO: Rename to AuthorisedScriptSTCS and move to ytxp-sdk

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldListSTCS = YieldListSTCS CurrencySymbol
  deriving newtype (Eq, IsString)

mkYieldListSTCS :: YieldListSTMPScript -> YieldListSTCS
mkYieldListSTCS (YieldListSTMPScript script) =
  YieldListSTCS $ CurrencySymbol (getScriptHash $ scriptHash script)

instance FromJSON YieldListSTCS where
  {-# INLINEABLE parseJSON #-}
  parseJSON =
    (pure . YieldListSTCS)
      <=< withText "YieldListSTCS" (pure . fromString . unpack)

instance ToJSON YieldListSTCS where
  {-# INLINEABLE toJSON #-}
  toJSON (YieldListSTCS cs) = toJSON . show $ cs

  {-# INLINEABLE toEncoding #-}
  toEncoding (YieldListSTCS cs) = toEncoding . show $ cs

--------------------------------------------------------------------------------
-- Helpers (Unexported)

{- |
  The `mkYieldListSTMPWrapper` is a YieldListST minting policy unapplied to its arguments,
  the script wraps another script, while performing additional checks of its own, which are outlined below.

  There are two potential validation branches: `Mint` and `Burn`.

  When the supplied redeemer is `Mint` the policy checks:
    - Exactly one token with an empty token name and the YieldListSTCS
      (as fetched from the `ScriptPurpose`) is minted
    - The minted token is sent to a UTxO at a script address
    - The UTxO receiving the minted token carries a valid `YieldList` in it's datum.
      In particular the following is checked:
        * The type fully decodes (using `PTryFrom`)
        * Hashes must be well-formed (of the correct length)
        * The yield list must be no larger than the max list size (provided as an argument)
        * No tokens except Ada and the YieldListSTMP are present at the recipient UTxO
    - Due to the wrapping script there is a potential for other script inputs and outputs beyond the ones
      accounted for in this script. There may also be other wallet inputs and outputs, depending on the
      type of wrapping script.

      We need to ensure that these other inputs or outputs do not carry a YieldListSTT:
        * We check that there is no wallet output carrying a YieldListSTT
          (We do not need to check the wallet inputs as a YieldListSTT is never
           sent to a wallet output to begin with)
        * We check that there is only one script output containing a YieldListSTT,
          the one minted in the transaction being validated
        * We check that no script input contains a YieldListSTT

  When the supplied redeemer is `Burn` the policy checks:
    - Exactly one token is burned
    - Exactly one UTxO carrying exactly one token with the `YieldListSTCS` appears at the input

  Note that the YLSTMP does not provide any additional security guarantees by itself.
  These must be library-user-defined via wrapping scripts.
  Some examples include:
    - initialSpend (one-shot)
    - multisig
    - admin sig
    - governance wrappers
-}
mkYieldListSTMPWrapper ::
  forall (s :: S).
  Natural ->
  Term
    s
    ( (PData :--> PScriptContext :--> POpaque)
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
mkYieldListSTMPWrapper
  maxYieldListSize =
    plam $ \scriptToWrap redeemer context' -> unTermCont $ do
      void $ pletC $ scriptToWrap # redeemer # context'

      let txInfo = pfromData $ pfield @"txInfo" # context'
          purpose = pfromData $ pfield @"purpose" # context'
          mint = pfromData $ pfield @"mint" # txInfo
          inputs = pfromData $ pfield @"inputs" # txInfo
          outputs = pfromData $ pfield @"outputs" # txInfo

      PMinting ((pfield @"_0" #) -> yieldListSymbol) <- pmatchC purpose

      yieldPolicyRedeemer <-
        pfromData . fst <$> ptryFromC @(PAsData PYieldListMPWrapperRedeemer) redeemer

      pure $ pmatch yieldPolicyRedeemer $ \case
        PMint -> unTermCont $ do
          pure $
            pif
              ( ptraceIfFalse
                  "mkYieldListSTMPWrapper failed"
                  ( pands
                      [ ptraceIfFalse
                          "Must mint exactly one YieldList token with an empty token name"
                          $ pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount
                            # mint
                            # yieldListSymbol
                            # pemptyTokenName
                            # 1
                      , -- We combine a few of the checks into one with this check for efficiency,
                        -- this check ensures that there is exactly one valid script output with a YieldListSTT,
                        -- it also ensures that no other script output contains a YieldListSTT,
                        -- and that no wallet output contains a YieldListSTT.
                        ptraceIfFalse
                          ( mconcat
                              [ "Must have exactly one valid script output with yield list token"
                              , ", and no other outputs with one or more yield list token"
                              ]
                          )
                          $ phasOnlyOneValidScriptOutputWithToken
                            maxYieldListSize
                            outputs
                            # yieldListSymbol
                      , ptraceIfFalse
                          "Cannot have script input that contains a YieldListSTT"
                          $ phasNoScriptInputWithToken
                            inputs
                            # yieldListSymbol
                      ]
                  )
              )
              (popaque $ pconstant ())
              perror
        PBurn -> unTermCont $ do
          pure $
            pif
              ( ptraceIfFalse
                  "mkYieldListSTMPWrapper failed"
                  ( pands
                      [ ptraceIfFalse
                          "Must burn exactly one yield list token"
                          $ pmintFieldHasTokenOfCurrencySymbolTokenNameAndAmount
                            # mint
                            # yieldListSymbol
                            # pemptyTokenName
                            # (-1)
                      , ptraceIfFalse
                          "Must have exactly one input that carries exactly one yield list token"
                          $ phasOnlyOneInputWithExactlyOneTokenWithSymbol
                            inputs
                            # yieldListSymbol
                      ]
                  )
              )
              (popaque $ pconstant ())
              perror

{- | Checks that the given 'PValue' contains the YieldListSTT
TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
-}
pcontainsYieldListSTT ::
  YieldListSTCS -> Term s (PValue anyKey anyAmount :--> PBool)
pcontainsYieldListSTT (YieldListSTCS symbol) =
  plam $ \value ->
    pmember # pconstant symbol # pto value
