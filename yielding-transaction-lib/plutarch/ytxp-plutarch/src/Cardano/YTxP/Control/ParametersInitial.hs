{-# LANGUAGE ImpredicativeTypes #-}

{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), object,
                   pairs, withObject, (.:), (.=))
import Numeric.Natural (Natural)
import Plutarch (Config)
import Plutarch.Api.V2 (PScriptContext)
import Prettyprinter (Pretty (pretty), braces, punctuate, sep, (<+>))

{- | Parameters available to the YieldListValidator and YieldListMP
during compilation (therefore not containing any script hashes).

This is a GADT because the nonces must be serializable (and thus haskell types)
as  well as able to be applied to plutarch scripts (and thus PTypes).

To load the `scriptToWrap` arguments, see @unsafeTermFromScript@ from
Cardano.YTxP.Control.Utils

@since 0.1.0
-}
data ControlParametersInitial (nonceType :: Type) =
  ControlParametersInitial {
    -- | If the yield list exceeds this size, blow up during STT minting
    -- @since 0.1.0
    maxYieldListSize :: !Natural,
    -- | A list of nonces for the yielding staking validators. One staking
    -- validator is compiled for each nonce.
    -- @since 0.1.0
    nonceList :: [nonceType],
    -- | The V2 script that the yield list MP will wrap. This might be an admin
    -- signature script, multisig script, etc.
    -- @since 0.1.0
    scriptToWrapYieldListMP :: forall (s :: S) .
      Term s (PData :--> PScriptContext :--> POpaque),
    -- | The V2 script that the yield list validator will wrap.
    scriptToWrapYieldListValidator :: forall (s :: S) .
      Term s (PData :--> PData :--> PScriptContext :--> POpaque),
    -- | Plutarch compilation config
    compilationConfig :: Config
    }

-- | @since 0.1.0
instance (Eq nonceType) => Eq (ControlParametersInitial nonceType) where
  {-# INLINEABLE (==) #-}
  cpi1 == cpi2 = let conf1 = compilationConfig cpi1
                     conf2 = compilationConfig cpi2 in
    equateConfig conf1 conf2 &&
    maxYieldListSize cpi1 == maxYieldListSize cpi2 &&
    nonceList cpi1 == nonceList cpi2 &&
    -- Note from Koz (11/03/24): If we get this far, the two Configs are equal,
    -- so it doesn't matter which one we use.
    equatePValidator conf1 (scriptToWrapYieldListValidator cpi1) (scriptToWrapYieldListValidator cpi2) &&
    equatePMintingPolicy conf1 (scriptToWrapYieldListMP cpi1) (scriptToWrapYieldListMP cpi2)

-- | @since 0.1.0
instance (Pretty nonceType) => Pretty (ControlParametersInitial nonceType) where
  {-# INLINEABLE pretty #-}
  pretty cpi = let conf = compilationConfig cpi in
    ("ControlParametersInitial" <+>) . braces . sep . punctuate "," $ [
      "maxYieldListSize:" <+> (pretty . maxYieldListSize $ cpi),
      "nonceList:" <+> (pretty . nonceList $ cpi),
      "scriptToWrapYieldListMP:" <+> prettyPMintingPolicy conf (scriptToWrapYieldListMP cpi),
      "scriptToWrapYieldListValidator:" <+> prettyPValidator conf (scriptToWrapYieldListValidator cpi),
      "compilationConfig:" <+> prettyConfig conf
      ]

-- | @since 0.1.0
instance ToJSON nonceType => ToJSON (ControlParametersInitial nonceType) where
  {-# INLINEABLE toJSON #-}
  toJSON cpi = let conf = compilationConfig cpi in
    object [
      "maxYieldListSize" .= maxYieldListSize cpi,
      "nonceList" .= nonceList cpi,
      -- Note from Koz (08/03/24): We need this _exact_ form or the compiler
      -- will complain with an unintelligible error message.
      "scriptToWrapYieldListMP" .= toJSONPMintingPolicy conf (scriptToWrapYieldListMP cpi),
      "scriptToWrapYieldListValidator" .= toJSONPValidator conf (scriptToWrapYieldListValidator cpi),
      "compilationConfig" .= WrappedConfig conf
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding cpi = let conf = compilationConfig cpi in
    pairs $
      "maxYieldListSize" .= maxYieldListSize cpi <>
      "nonceList" .= nonceList cpi <>
      "scriptToWrapYieldListMP" .= toJSONPMintingPolicy conf (scriptToWrapYieldListMP cpi) <>
      "scriptToWrapYieldListValidator" .= toJSONPValidator conf (scriptToWrapYieldListValidator cpi) <>
      "compilationConfig" .= WrappedConfig conf

-- | @since 0.1.0
instance FromJSON nonceType => FromJSON (ControlParametersInitial nonceType) where
  {-# INLINEABLE parseJSON #-}
  -- Note from Koz (08/03/24): We have to write this method in such a convoluted
  -- way because we have to return impredicatively from the parser helpers for
  -- the closed Plutarch terms. While this is possible with ImpredicativeTypes
  -- enabled, this is currently broken with 'do' notation, and has been for a
  -- while. We thus isolate the impredicatively instantiated variables inside
  -- manual binds, and then do the rest in a 'do' for convenience.
  --
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/18126#note_423208.
  parseJSON = withObject "ControlParametersInitial" $ \obj ->
    (obj .: "scriptToWrapYieldListMP") >>=
    parseJSONPMintingPolicy >>=
    \mp -> (obj .: "scriptToWrapYieldListValidator") >>=
    parseJSONPValidator >>=
    \v -> do
        cpiMaxYieldListSize <- obj .: "maxYieldListSize"
        cpiNonceList <- obj .: "nonceList"
        WrappedConfig cpiConfig <- obj .: "compilationConfig"
        pure $ ControlParametersInitial cpiMaxYieldListSize
                                        cpiNonceList
                                        mp
                                        v
                                        cpiConfig
