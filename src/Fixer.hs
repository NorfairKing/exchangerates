{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Fixer
    ( fixer
    ) where

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client
import System.Exit
import Text.Read
import Web.HttpApiData

fixer :: IO ()
fixer = do
    man <- newManager defaultManagerSettings
    burl <- parseBaseUrl "http://api.fixer.io/"
    res <- runClientM (getLatest Nothing Nothing) (ClientEnv man burl)
    case res of
        Left err -> die $ show err
        Right v -> print v

fixerAPI :: Proxy FixerAPI
fixerAPI = Proxy

type FixerAPI = GetLatest :<|> GetAtDate

type GetLatest
     = "latest" :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

type GetAtDate
     = Capture "date" Day :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

getLatest :: Maybe Currency -> Maybe Symbols -> ClientM Rates
getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> ClientM Rates
getLatest :<|> getAtDate = client fixerAPI

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
    deriving (Show, Read, Eq, Ord)

instance ToHttpApiData Currency where
    toUrlPiece = T.pack . show

newtype Symbols = Symbols
    { unSymbols :: NonEmpty Currency
    } deriving (Show, Eq, Ord)

instance ToHttpApiData Symbols where
    toUrlPiece = T.intercalate "," . map toUrlPiece . NE.toList . unSymbols

data Rates = Rates
    { responseBase :: Currency
    , responseDate :: Day
    , responseRates :: Map Currency Double
    } deriving (Show, Eq)

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

instance FromJSON Rates where
    parseJSON =
        withObject "Rates" $ \o ->
            Rates <$> o .: "base" <*> o .: "date" <*> o .: "rates"
