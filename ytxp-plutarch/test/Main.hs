module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

import Cardano.YTxP.Test.Control.Yielding.Helper qualified as YieldingHelper
import Utils qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "YTxP-lib Test Suite"
      [ YieldingHelper.tests
      , Utils.tests
      ]
