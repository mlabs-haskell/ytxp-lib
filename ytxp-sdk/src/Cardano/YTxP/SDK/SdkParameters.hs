{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.SDK.SdkParameters (
  SdkParameters (..),
  AuthorisedScriptsSTCS (..),
) where

import Control.Monad ((<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  withText,
 )
import Data.Text (unpack)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import PlutusCore (DefaultUni)
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol))
import PlutusTx qualified
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prettyprinter (
  Pretty,
  align,
  braces,
  dquotes,
  pretty,
  punctuate,
  vsep,
  (<+>),
 )

-- | Parameters available during compilation (therefore not containing any script hashes).
data SdkParameters = SdkParameters
  { validatorsNonceList :: [Natural]
  -- ^ A list of nonces for the validators. One validator is compiled for each nonce.
  -- @since 0.2.1
  , authorisedScriptsSTCS :: AuthorisedScriptsSTCS
  -- ^ The Currency symbol of the token that identifies authorised reference scripts .
  -- @since 0.1.0
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty SdkParameters where
  pretty
    SdkParameters
      { validatorsNonceList
      , authorisedScriptsSTCS
      } =
      ("SdkParameters:" <+>) . braces . align . vsep . punctuate "," $
        [ "validatorsNonceList:" <+> pretty validatorsNonceList
        , "authorisedScriptsSTCS:" <+> dquotes (pretty authorisedScriptsSTCS)
        ]

-- | Semantic newtype for the YieldList state thread currency symbol
newtype AuthorisedScriptsSTCS = AuthorisedScriptsSTCS CurrencySymbol
  deriving newtype
    ( Eq
    , Show
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    , PlutusTx.Typeable DefaultUni
    , PlutusTx.Lift DefaultUni
    , Pretty
    )

instance FromJSON AuthorisedScriptsSTCS where
  {-# INLINEABLE parseJSON #-}
  parseJSON =
    (pure . AuthorisedScriptsSTCS)
      <=< withText
        "AuthorisedScriptsSTCS"
        (pure . CurrencySymbol . stringToBuiltinByteStringHex . unpack)

instance ToJSON AuthorisedScriptsSTCS where
  {-# INLINEABLE toJSON #-}
  toJSON (AuthorisedScriptsSTCS cs) = toJSON . show $ cs

  {-# INLINEABLE toEncoding #-}
  toEncoding (AuthorisedScriptsSTCS cs) = toEncoding . show $ cs
