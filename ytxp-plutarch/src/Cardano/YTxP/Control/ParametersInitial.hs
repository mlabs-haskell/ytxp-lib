{-# LANGUAGE ImpredicativeTypes #-}

{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.Control.ParametersInitial (
  SdkParameters (..),
) where

import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )
import Numeric.Natural (Natural)
import Plutarch (Config)
import Plutarch.Api.V2 (PScriptContext)
import PlutusLedgerApi.V2 (CurrencySymbol)
import Prettyprinter (Pretty (pretty), braces, punctuate, sep, (<+>))

{- | Parameters available to the YieldListValidator and YieldListMP
during compilation (therefore not containing any script hashes).

This is a GADT because the nonces must be serializable (and thus haskell types)
as well as able to be applied to plutarch scripts (and thus PTypes).

To load the `scriptToWrap` arguments, see @unsafeTermFromScript@ from
Cardano.YTxP.Control.Utils

@since 0.1.0
-}
data SdkParameters = SdkParameters
  { stakingValidatorsNonceList :: [Natural]
  -- ^ A list of nonces for the yielding staking validators. One staking
  -- validator is compiled for each nonce.
  -- @since 0.1.0
  , mintingPoliciesNonceList :: [Natural]
  -- ^ A list of nonces for the yielding minting policies. One minting
  -- policy is compiled for each nonce.
  -- @since 0.1.0
  , authorisedScriptsSTCS :: YieldListSTCS
  -- ^ The Currency symbol of the token that identifies authorised reference scripts .
  -- @since 0.1.0
  , compilationConfig :: Config
  -- ^ Plutarch compilation config
  }

-- | @since 0.1.0
instance Eq SdkParameters where
  {-# INLINEABLE (==) #-}
  cpi1 == cpi2 =
    let conf1 = compilationConfig cpi1
        conf2 = compilationConfig cpi2
     in equateConfig conf1 conf2
          && stakingValidatorsNonceList cpi1 == stakingValidatorsNonceList cpi2
          && mintingPoliciesNonceList cpi1 == mintingPoliciesNonceList cpi2
          && authorisedScriptsSTCS cpi1 == authorisedScriptsSTCS cpi2

-- | @since 0.1.0
instance Pretty SdkParameters where
  {-# INLINEABLE pretty #-}
  pretty cpi =
    let conf = compilationConfig cpi
     in ("SdkParameters" <+>) . braces . sep . punctuate "," $
          [ "stakingValidatorsNonceList:" <+> (pretty . stakingValidatorsNonceList $ cpi)
          , "mintingPoliciesNonceList:" <+> (pretty . mintingPoliciesNonceList $ cpi)
          , "compilationConfig:" <+> prettyConfig conf
          ]

-- | @since 0.1.0
instance ToJSON SdkParameters where
  {-# INLINEABLE toJSON #-}
  toJSON cpi =
    let conf = compilationConfig cpi
     in object
          [ "stakingValidatorsNonceList" .= stakingValidatorsNonceList cpi
          , "mintingPoliciesNonceList" .= mintingPoliciesNonceList cpi
          , "compilationConfig" .= WrappedConfig conf
          ]
  {-# INLINEABLE toEncoding #-}
  toEncoding cpi =
    let conf = compilationConfig cpi
     in pairs $
          "stakingValidatorsNonceList" .= stakingValidatorsNonceList cpi
            <> "mintingPoliciesNonceList" .= mintingPoliciesNonceList cpi
            <> "compilationConfig" .= WrappedConfig conf

-- | @since 0.1.0
instance FromJSON SdkParameters where
  {-# INLINEABLE parseJSON #-}
  -- Note from Koz (08/03/24): We have to write this method in such a convoluted
  -- way because we have to return impredicatively from the parser helpers for
  -- the closed Plutarch terms. While this is possible with ImpredicativeTypes
  -- enabled, this is currently broken with 'do' notation, and has been for a
  -- while. We thus isolate the impredicatively instantiated variables inside
  -- manual binds, and then do the rest in a 'do' for convenience.
  --
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/18126#note_423208.
  parseJSON = withObject "SdkParameters" $ \obj -> do
    sdkStakingValidatorsNonceList <- obj .: "stakingValidatorsNonceList"
    sdkMintingPoliciesNonceList <- obj .: "mintingPoliciesNonceList"
    sdkAuthorisedScriptsSTCS <- obj .: "authorisedScriptsSTCS"

    WrappedConfig sdkConfig <- obj .: "compilationConfig"
    pure $
      SdkParameters
        sdkStakingValidatorsNonceList
        sdkMintingPoliciesNonceList
        sdkAuthorisedScriptsSTCS
        sdkConfig
