module Cardano.YTxP (
  YTxPParams,
  validatorLinker,
  stakeValidatorLinker,
  mintingPolicyLinker,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Map (fromList)
import Data.Text (Text, pack)

import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (
    authorisedScriptsSTCS,
    mintingPoliciesNonceList,
    stakingValidatorsNonceList
  ),
 )

import PlutusLedgerApi.V2 (CurrencySymbol)
import Ply qualified

import ScriptExport.ScriptInfo (
  Linker,
  ScriptExport (ScriptExport),
  ScriptRole (ThreeArgumentScript, TwoArgumentScript),
  fetchTS,
  getParam,
  toRoledScript,
 )
import Ply.Core.Unsafe (unsafeUnTypedScript', unsafeTypedScript)
import UntypedPlutusCore (applyProgram)
import Plutarch (Script(Script), compile, Config (NoTracing))
import PlutusPrelude (unsafeFromRight)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)

--------------------------------------------------------------------------------

data YTxPParams = YTxPParams
  { params :: SdkParameters
  , commitHash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply a Plutarch (Haskell lifted) term to a script
-- | We use it instead of Ply.# due to issues with encoding encountered. 
ap :: PUnsafeLiftDecl x => Ply.TypedScript r (PLifted x ': xs) -> PLifted x -> Ply.TypedScript r xs
ap ts x = unsafeTypedScript ver $ unsafeFromRight $ prog `applyProgram` xc
  where (ver, prog) = unsafeUnTypedScript' ts
        Script xc = unsafeFromRight $ compile NoTracing $ pconstant x


validatorLinker :: Linker SdkParameters (ScriptExport SdkParameters)
validatorLinker = do
  info <- getParam

  yieldingValidator <-
    fetchTS
      @'ThreeArgumentScript
      @'[CurrencySymbol]
      "ytxp:yieldingValidator"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS info)

    yieldingValidator' =
      yieldingValidator `ap` authorisedScriptsSymbol

  return $
    ScriptExport
      ( fromList
          [("ytxp:yieldingValidator", toRoledScript yieldingValidator')]
      )
      info

mintingPolicyLinker :: Linker SdkParameters (ScriptExport SdkParameters)
mintingPolicyLinker = do
  info <- getParam

  yieldingMP <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingMintingPolicy"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS info)

    yieldingMPs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingMintingPolicy:" <> show nonce)
            , toRoledScript $
                yieldingMP `ap` authorisedScriptsSymbol `ap` toInteger nonce
            )
        )
        (mintingPoliciesNonceList info)

  return $
    ScriptExport
      (fromList yieldingMPs)
      info

stakeValidatorLinker :: Linker SdkParameters (ScriptExport SdkParameters)
stakeValidatorLinker = do
  info <- getParam

  yieldingSV <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingStakeValidator"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS info)

    yieldingSVs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingStakeValidator:" <> show nonce)
            , toRoledScript $
                yieldingSV `ap` authorisedScriptsSymbol `ap` toInteger nonce
            )
        )
        (stakingValidatorsNonceList info)

  return $
    ScriptExport
      (fromList yieldingSVs)
      info
