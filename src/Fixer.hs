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

import Fixer.Types

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
