{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the API
module ExchangeRates.Types
    ( Rate(..)
    , oneRate
    , mulRate
    , divRate
    , normaliseRate
    , rateToRational
    , rateToDouble
    , Currency(..)
    , Symbols(..)
    , Rates(..)
    ) where

import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import GHC.Natural
import GHC.Real (Ratio(..))
import Servant.API
import Text.Read

-- | A positive and non-0 ratio
newtype Rate = Rate
    { unRate :: Ratio Natural
    } deriving (Show, Eq, Generic)

-- | The identity of 'mulRate'
oneRate :: Rate
oneRate = Rate 1

-- | Multiply two rates
mulRate :: Rate -> Rate -> Rate
mulRate (Rate r1) (Rate r2) = normaliseRate $ Rate (r1 * r2)

-- | Divide one rate by another
divRate :: Rate -> Rate -> Rate
divRate (Rate r1) (Rate r2) = normaliseRate $ Rate (r1 / r2)

-- | Ensure that a rate is internally normalised.
-- This is a requirement for 'Validity'
normaliseRate :: Rate -> Rate
normaliseRate = Rate . fromRational . toRational . unRate

-- | Convert a rate to a 'Ratio Integer' instead of 'Ratio Natural'.
--
-- This is a lossless transformation.
rateToRational :: Rate -> Rational
rateToRational (Rate (num :% den)) = fromIntegral num :% fromIntegral den

-- | Convert a rate to a 'Double'.
--
-- This may not be a lossless transformation
rateToDouble :: Rate -> Double
rateToDouble = fromRational . rateToRational

-- | A rate is valid if:
--
-- * The ratio inside is valid.
-- * The ratio is not zero.
-- * The ratio is normalised.
instance Validity Rate where
    validate r@Rate {..} =
        mconcat
            [ unRate <?!> "unRate"
            , (numerator unRate /= 0) <?@> "ratio is not 0"
            , (normaliseRate r == r) <?@> "is normalised"
            ]
    isValid = isValidByValidating

instance FromJSON Rate where
    parseJSON v =
        (Rate <$> parseJSON v) <|>
        -- First try to parse as a ratio
        (do d <- parseJSON v :: JSON.Parser Double
            -- The api has a precision of 0.0001.
            let r@(num :% den) = toRational d
            if r == 0
                then fail "Failed to parse zero rate"
                else pure $
                     normaliseRate $
                     Rate $ naturalFromInteger num % naturalFromInteger den)

instance ToJSON Rate where
    toJSON = toJSON . unRate

-- | A sum-type of the supported currencies on fixer.io
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

-- | A nonempty list of 'Currency's
newtype Symbols = Symbols
    { unSymbols :: NonEmpty Currency
    } deriving (Show, Eq, Ord, Generic)

instance Validity Symbols

instance ToHttpApiData Symbols where
    toUrlPiece = T.intercalate "," . map toUrlPiece . NE.toList . unSymbols

-- | The raw response of fixer.io
data Rates = Rates
    { ratesBase :: Currency
    , ratesDate :: Day
    , ratesRates :: Map Currency Rate
    } deriving (Show, Eq, Generic)

instance Validity Rates where
    validate Rates {..} =
        mconcat
            [ ratesBase <?!> "ratesBase"
            , ratesDate <?!> "ratesDate"
            , ratesRates <?!> "ratesRates"
            , isNothing (M.lookup ratesBase ratesRates) <?@>
              "does not contain the base rate in the rates."
            ]
    isValid = isValidByValidating

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
