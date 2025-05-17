{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Main
Description : Main module for the YTxP compiler.

This module provides the entry point for the YTxP compiler, which generates a blueprint
for the Yielding Transaction Protocol (YTxP). It includes command-line options for
configuring the compiler and generating the blueprint.
-}
module Main (main) where

import Cardano.YTxP (ytxpBlueprint)
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (SdkParameters),
 )
import Numeric.Natural (Natural)
import Options.Applicative (
  Parser,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  option,
  short,
  showDefault,
  strOption,
  switch,
  value,
  (<**>),
 )
import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogInfo),
  TracingMode (DetTracing),
 )
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol))
import PlutusTx.Blueprint (writeBlueprint)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)

{- |
Data type representing the parameters for the YTxP compiler.
-}
data Params = Params
  { outputFile :: !FilePath
  , numYieldingSV :: !Natural
  , numYieldingMP :: !Natural
  , numYieldingCV :: !Natural
  , numYieldingVV :: !Natural
  , numYieldingPV :: !Natural
  , initialNonce :: !Natural
  , stcs :: !CurrencySymbol
  , traces :: !Bool
  }

{- |
Starts the YTxP compiler with the given parameters.
-}
start :: Params -> IO ()
start p =
  let
    sdkParams = params2SdkParameters p
    config =
      if traces p then Tracing LogInfo DetTracing else NoTracing
   in
    writeBlueprint
      (outputFile p)
      (ytxpBlueprint config sdkParams)

{- |
Converts the given parameters to SDK parameters.
-}
params2SdkParameters :: Params -> SdkParameters
params2SdkParameters
  Params
    { numYieldingSV
    , numYieldingMP
    , numYieldingCV
    , numYieldingVV
    , numYieldingPV
    , initialNonce
    , stcs
    } =
    SdkParameters
      (f initialNonce numYieldingSV)
      ( f
          ( initialNonce
              + numYieldingSV
          )
          numYieldingMP
      )
      ( f
          ( initialNonce
              + numYieldingSV
              + numYieldingMP
          )
          numYieldingCV
      )
      ( f
          ( initialNonce
              + numYieldingSV
              + numYieldingMP
              + numYieldingCV
          )
          numYieldingVV
      )
      ( f
          ( initialNonce
              + numYieldingSV
              + numYieldingMP
              + numYieldingCV
              + numYieldingVV
          )
          numYieldingPV
      )
      (AuthorisedScriptsSTCS stcs)
    where
      f :: Natural -> Natural -> [Natural]
      f n k = [n .. n + k - 1]

-- CLI Parser

{- |
Entry point for the YTxP compiler.
-}
main :: IO ()
main = start =<< execParser opts
  where
    opts =
      info
        (params <**> helper)
        ( fullDesc
            <> header "ytxp-lib - YTxP Compiler"
        )

{- |
Instance of the Read type class for CurrencySymbol.
-}
instance Read CurrencySymbol where
  readsPrec _ cs =
    [(CurrencySymbol . stringToBuiltinByteStringHex $ cs, mempty)]

{- |
Parser for the command-line parameters.
-}
params :: Parser Params
params =
  Params
    <$> strOption
      ( long "output"
          <> short 'o'
          <> help "Output blueprint file"
      )
    <*> option
      auto
      ( long "yielding-staking-validator-number"
          <> short 's'
          <> help "The number of yielding staking validators"
          <> value 0
      )
    <*> option
      auto
      ( long "yielding-minting-policy-number"
          <> short 'm'
          <> help "The number of yielding minting policies"
          <> value 0
      )
    <*> option
      auto
      ( long "yielding-certifying-validator-number"
          <> short 'c'
          <> help "The number of yielding certifying validators"
          <> value 0
      )
    <*> option
      auto
      ( long "yielding-voting-validator-number"
          <> short 'v'
          <> help "The number of yielding voting validators"
          <> value 0
      )
    <*> option
      auto
      ( long "yielding-proposing-validator-number"
          <> short 'p'
          <> help "The number of yielding proposing validators"
          <> value 0
      )
    <*> option
      auto
      ( long "initial-nonce"
          <> help "The initial nonce value"
          <> showDefault
          <> value 42
      )
    <*> option
      auto
      ( long "stcs"
          <> help "The authorised scripts STCS"
      )
    <*> switch
      ( long "traces"
          <> help "Whether to compile with traces"
      )
