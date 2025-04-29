module Cardano.YTxP (
  validatorLinker,
  stakeValidatorLinker,
  mintingPolicyLinker,
) where

import Data.Coerce (coerce)
import Data.Map (fromList)
import Data.Text (pack)

import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (
    authorisedScriptsSTCS,
    mintingPoliciesNonceList,
    stakingValidatorsNonceList
  ),
 )

import PlutusLedgerApi.V3 (CurrencySymbol)
import qualified Ply

import Plutarch.Internal.Term (Config (NoTracing), Script (Script), compile)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import PlutusPrelude (unsafeFromRight)
import Ply.Core.Unsafe (unsafeTypedScript, unsafeUnTypedScript')
import ScriptExport.ScriptInfo (
  Linker,
  ScriptExport (ScriptExport),
  ScriptRole (ThreeArgumentScript, TwoArgumentScript),
  fetchTS,
  getParam,
  toRoledScript,
 )
import UntypedPlutusCore (applyProgram)

--------------------------------------------------------------------------------

{- | Apply a Plutarch (Haskell lifted) term to a script
| We use it instead of Ply.# due to issues with encoding encountered.
-}
ap :: (PUnsafeLiftDecl x) => Ply.TypedScript r (PLifted x ': xs) -> PLifted x -> Ply.TypedScript r xs
ap ts x = unsafeTypedScript ver $ unsafeFromRight $ prog `applyProgram` xc
  where
    (ver, prog) = unsafeUnTypedScript' ts
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
