module Cardano.YTxP.Control.Yielding.Validator (
  -- * Validator
  YieldingValidatorScript (getYieldingValidatorScript),
  compileYieldingValidator,

  -- * Script Credential
  YieldingValidatorCredential,
  mkYieldingValidatorCredential,
) where

import Cardano.YTxP.Control.YieldList (PYieldedToHash (PYieldedToMP, PYieldedToSV, PYieldedToValidator))
import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Cardano.YTxP.Control.Yielding (getYieldedToHash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V1.Address (PCredential (PScriptCredential))
import Plutarch.Api.V2 (PScriptContext, PStakingCredential (PStakingHash),
                        scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Utils (pmember, pscriptHashToCurrencySymbol)

--------------------------------------------------------------------------------
-- Yielding Validator Script

-- | @since 0.1.0
newtype YieldingValidatorScript = YieldingValidatorScript
  { -- | @since 0.1.0
    getYieldingValidatorScript :: Script
  }
  deriving (
    -- | @since 0.1.0
    ToJSON,
    -- | @since 0.1.0
    FromJSON
    ) via (HexStringScript "YieldingValidatorScript")

compileYieldingValidator ::
  Config ->
  YieldListSTCS ->
  (Either Text)
    YieldingValidatorScript
compileYieldingValidator config ylstcs = do
  let
    yieldingValidator ::
      forall (s :: S).
      ( Term s (PData :--> PData :--> PScriptContext :--> POpaque)
      )
    yieldingValidator = mkYieldingValidator ylstcs

  script <- compile config yieldingValidator
  pure $ YieldingValidatorScript script

-------------------------------------------------------------------------------
-- Yielding Validator Credential

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldingValidatorCredential = YieldingValidatorCredential Credential

mkYieldingValidatorCredential ::
  YieldingValidatorScript -> YieldingValidatorCredential
mkYieldingValidatorCredential (YieldingValidatorScript script) =
  YieldingValidatorCredential $ ScriptCredential (scriptHash script)

--------------------------------------------------------------------------------
-- Helpers (Unexported)

-- -   Look at the UTxO at the `n` th entry in the `txInfoReferenceInputs`, where `n` is equal to `yieldListInputIndex`.
--     -   Call this UTxO `yieldListUTxO`.
--     -   Check that this UTxO is carrying exactly one token with the `yieldListSTCS`. Blow up if not.
-- -   "Unsafely" deserialize the datum of the `yieldListUTxO` to a value `yieldList :: YieldList`
-- -   Grab the correct `YieldToHash` by looking at the `n` th entry of `yieldList`, where `n` is equal to
--     `yieldListIndex`. Call this hash `yieldToHash`.
-- -   Obtain evidence that the a script with `yieldToHash` was triggered via the `checkYieldList` function.
--     If not, blow up. In practice, this will involve either:
--     -   Looking at the `txInfoWithdrawls` field for a staking validator being triggered with the correct StakingCredential
--     -   Looking at the `txInfoInputs` field for a UTxO being spent at the correct address
--     -   Looking at the `txInfoMints` field for a mint with the correct currency symbol

mkYieldingValidator ::
  forall (s :: S).
  YieldListSTCS ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
mkYieldingValidator ylstcs = plam $ \_datum redeemer ctx -> unTermCont $ do
  txInfo <- pletC $ pfromData $ pfield @"txInfo" # ctx
  let txInfoRefInputs = pfromData $ pfield @"referenceInputs" # txInfo
  yieldingRedeemer <- pfromData . fst <$> ptryFromC redeemer
  let yieldToHash = getYieldedToHash ylstcs # txInfoRefInputs # yieldingRedeemer

  pure $
    popaque $
      pmatch yieldToHash $ \case
        PYieldedToValidator ((pfield @"scriptHash" #) -> hash') ->
          let txInfoInputs = pfromData $ pfield @"inputs" # txInfo
           in ptraceIfFalse "No input found" $
                pany
                  # ( plam $ \input ->
                        let out = pfield @"resolved" # input
                            address = pfield @"address" # pfromData out
                            credential = pfield @"credential" # pfromData address
                         in pmatch (pfromData credential) $ \case
                              PScriptCredential ((pfield @"_0" #) -> hash) -> hash #== hash'
                              _ -> pconstant False
                    )
                  # txInfoInputs
        PYieldedToMP ((pfield @"scriptHash" #) -> hash') ->
          let txInfoMints = pfromData $ pfield @"mint" # txInfo
              currencySymbol = pscriptHashToCurrencySymbol hash'
           in ptraceIfFalse "No minting policy found" $
                pmember # currencySymbol # (pto txInfoMints)

        PYieldedToSV ((pfield @"scriptHash" #) -> hash') ->
          let txInfoWithdrawls = pfromData $ pfield @"wdrl" # txInfo
           in precList
                ( \self x xs ->
                    pmatch (pfromData $ pfstBuiltin # x) $ \case
                      PStakingHash ((pfield @"_0" #) -> credential) ->
                        pmatch credential $ \case
                          PScriptCredential ((pfield @"_0" #) -> hash) -> hash #== hash' #|| self # xs
                          _ -> self # xs
                      _ -> self # xs
                )
                (const $ ptraceError "No staking validator found")
                # (pto txInfoWithdrawls)
