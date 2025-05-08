{-# OPTIONS_GHC -Wno-orphans #-}

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

data Params = Params
  { outputFile :: !FilePath
  , numYieldingSV :: !Natural
  , numYieldingMP :: !Natural
  , initialNonce :: !Natural
  , stcs :: !CurrencySymbol
  , traces :: !Bool
  }

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

params2SdkParameters :: Params -> SdkParameters
params2SdkParameters Params {numYieldingSV, numYieldingMP, initialNonce, stcs} =
  SdkParameters
    (f initialNonce numYieldingSV)
    (f (initialNonce + numYieldingSV) numYieldingMP)
    (AuthorisedScriptsSTCS stcs)
  where
    f :: Natural -> Natural -> [Natural]
    f n k = [n .. n + k - 1]

-- CLI Parser

main :: IO ()
main = start =<< execParser opts
  where
    opts =
      info
        (params <**> helper)
        ( fullDesc
            <> header "ytxp-lib - YTxP Compiler"
        )

instance Read CurrencySymbol where
  readsPrec _ cs =
    [(CurrencySymbol . stringToBuiltinByteStringHex $ cs, mempty)]

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
