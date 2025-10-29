{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.YTxP.SDK.Optics (HasYieldingRedeemer (..)) where

import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex,
  AuthorisedScriptProofIndex,
  YieldingRedeemer,
 )

import Control.Lens (makeClassyFor, makeWrapped)

makeWrapped ''AuthorisedScriptProofIndex

makeWrapped ''AuthorisedScriptIndex

makeClassyFor
  "HasYieldingRedeemer"
  "yieldingRedeemer"
  [ ("authorisedScriptIndex", "authorisedScriptIndex")
  , ("authorisedScriptProofIndex", "authorisedScriptProofIndex")
  ]
  ''YieldingRedeemer
