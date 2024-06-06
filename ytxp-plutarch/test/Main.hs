module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

import Cardano.YTxP.Test.Control.Yielding.Scripts qualified as YieldingScripts

import Utils qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "YTxP-lib Test Suite"
      [ YieldingScripts.tests
      , Utils.tests
      ]
