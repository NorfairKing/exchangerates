{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixer.Types
    ( Currency(..)
    , Symbols(..)
    , Rates(..)
    ) where

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Servant.API
import Text.Read

data Currency
    = AUD
    | BGN
    | BRL
    | CAD
    | CHF
    | CNY
    | CZK
    | DKK
    | EUR
    | GBP
    | HKD
    | HRK
    | HUF
    | IDR
    | ILS
    | INR
    | JPY
    | KRW
    | MXN
    | MYR
    | NOK
    | NZD
    | PHP
    | PLN
    | RON
    | RUB
    | SEK
    | SGD
    | THB
    | TRY
    | USD
    | ZAR
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Validity Currency

instance ToHttpApiData Currency where
    toUrlPiece = currencyToText

newtype Symbols = Symbols
    { unSymbols :: NonEmpty Currency
    } deriving (Show, Eq, Ord, Generic)

instance Validity Symbols

instance ToHttpApiData Symbols where
    toUrlPiece = T.intercalate "," . map toUrlPiece . NE.toList . unSymbols

data Rates = Rates
    { responseBase :: Currency
    , responseDate :: Day
    , responseRates :: Map Currency Double
    } deriving (Show, Eq, Generic)

instance Validity Rates

instance FromJSON Currency where
    parseJSON = withText "Currency" currencyTextParser

instance FromJSONKey Currency where
    fromJSONKey = FromJSONKeyTextParser currencyTextParser

currencyTextParser :: Text -> JSON.Parser Currency
currencyTextParser t =
    let s = T.unpack t
    in case readMaybe s of
           Nothing -> fail $ "Not a valid currency: " ++ s
           Just c -> pure c

instance ToJSON Currency where
    toJSON = JSON.String . currencyToText

instance ToJSONKey Currency where
    toJSONKey = toJSONKeyText currencyToText

currencyToText :: Currency -> Text
currencyToText = T.pack . show

instance FromJSON Rates where
    parseJSON =
        withObject "Rates" $ \o ->
            Rates <$> o .: "base" <*> o .: "date" <*> o .: "rates"
