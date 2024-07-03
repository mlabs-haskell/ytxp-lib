module Main (main) where

import Test.Tasty (defaultMain, testGroup)

-- TODO these tests were placeholders,
-- actual tests are: https://github.com/mlabs-haskell/ytxp-lib/pull/17
main :: IO ()
main = do
  defaultMain $
    testGroup
      "test suite"
      []
