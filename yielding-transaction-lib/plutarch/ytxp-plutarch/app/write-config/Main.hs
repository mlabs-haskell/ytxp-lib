module Main (main) where

import Cardano.YTxP.Control.Parameters (mkControlParameters)
import Cardano.YTxP.Control.ParametersInitial (ControlParametersInitial)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "No file path provided"
    [fp] -> do
      fileExists <- doesFileExist fp
      if fileExists
      then do
        decoded <- eitherDecodeFileStrict fp
        case decoded of
          Left err -> error $ "Could not decode JSON at path " <> show fp <> "\n" <> show err
          Right (cpi :: ControlParametersInitial Integer) -> case mkControlParameters cpi of
            Left err -> error $ "Could not make ControlParameters: " <> show err
            Right cp -> do
              let encoded = BSL.toStrict . encodePretty $ cp
              BS.putStr encoded
              putStrLn "" -- so we have a nice linebreak
      else error $ "No file exists at path: " <> show fp
    _ -> error "Too many arguments: want a single file path"
