{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial (..),
) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Numeric.Natural (Natural)
import Plutarch (Config)
import Plutarch.Lift (PConstantDecl, PConstanted, PLifted)
import Plutarch.Api.V2 (PScriptContext)

{- | Parameters available to the YieldListValidator and YieldListMP
during compilation (therefore not containing any script hashes).

This is a GADT because the nonces must be serializable (and thus haskell types)
as  well as able to be applied to plutarch scripts (and thus PTypes).

To load the `scriptToWrap` arguments, see @unsafeTermFromScript@ from
Cardano.YTxP.Control.Utils

Docs on the fields (apparently haddocks don't work as usual with GADT syntax?)

   { maxYieldListSize :: !Natural
    -- ^ If the yield list exceeds this size, blow up during STT minting
    , nonceList :: ![nonceType]
    -- ^ a list of nonces for the yielding staking validators.
    -- One staking validator is compiled for each nonce
    , scriptToWrapYieldListMP ::
        !(ClosedTerm (PData :--> PScriptContext :--> POpaque))
    -- ^ The V2 script that the Yield List MP will wrap. This might be an admin
    -- signature script, multisig script, etc.
    , scriptToWrapYieldListValidator ::
        !(ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque))
    -- ^ The V2 script that the Yield List MP will wrap. This might be an admin
    -- signature script, multisig script, etc.
    , compilationConfig :: Config
    -- ^ Plutarch compilation config
    }
-}
data ControlParametersInitial (nonceType :: Type) where
  ControlParametersInitial ::
    ( PConstantDecl nonceType
    , nonceType ~ PLifted (PConstanted nonceType)
    ) =>
    { maxYieldListSize :: !Natural
    , nonceList :: ![nonceType]
    , scriptToWrapYieldListMP ::
        !(ClosedTerm (PData :--> PScriptContext :--> POpaque))
    , scriptToWrapYieldListValidator ::
        !(ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque))
    , compilationConfig :: Config
    } ->
    ControlParametersInitial nonceType

instance ToJSON nonceType => ToJSON (ControlParametersInitial nonceType) where
  toJSON = error "unimplemented"

instance FromJSON nonceType => FromJSON (ControlParametersInitial nonceType) where
  parseJSON = error "unimplemented"
