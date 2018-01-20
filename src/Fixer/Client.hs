{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fixer.Client
    ( autoRunFixerClient
    , runFixerClient
    , FClient
    , getLatest
    , getAtDate
    , readCacheFromFile
    , flushCacheToFile
    ) where

import Control.Monad.State
import Data.Maybe
import Data.Time
import qualified Data.Yaml as Yaml
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client

import qualified Fixer.API as Raw
import Fixer.Cache
import Fixer.Types

newtype FClient a = FClient
    { runFClient :: StateT FixerCache ClientM a
    } deriving (Functor, Applicative, Monad, MonadState FixerCache, MonadIO)

autoRunFixerClient :: FClient a -> IO (Either ServantError a)
autoRunFixerClient func = do
    man <- newManager defaultManagerSettings
    burl <- parseBaseUrl "http://api.fixer.io/"
    runFixerClient emptyFixerCache (ClientEnv man burl) func

runFixerClient ::
       FixerCache -> ClientEnv -> FClient a -> IO (Either ServantError a)
runFixerClient fc ce func = runClientM (evalStateT (runFClient func) fc) ce

getLatest :: Maybe Currency -> Maybe Symbols -> FClient Rates
getLatest mc ms = do
    date <- liftIO $ utctDay <$> getCurrentTime
    withCache date mc ms $ Raw.getLatest mc ms

getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> FClient Rates
getAtDate date mc ms = withCache date mc ms $ Raw.getAtDate date mc ms

withCache ::
       Day -> Maybe Currency -> Maybe Symbols -> ClientM Rates -> FClient Rates
withCache date mc ms func = do
    let base = fromMaybe defaultBaseCurrency mc
    let symbols = fromMaybe allSymbols ms
    c <- get
    case lookupRatesInCache date base symbols c of
        Nothing ->
            FClient $ do
                liftIO $ putStrLn "Nothing found in cache, fetching."
                rates <- lift func
                modify (insertRatesInCache rates)
                pure rates
        Just rates -> pure rates

readCacheFromFile :: FilePath -> FClient ()
readCacheFromFile fp = do
    mfc <- liftIO $ Yaml.decodeFile fp
    case mfc of
        Nothing -> pure ()
        Just fc -> put fc

flushCacheToFile :: FilePath -> FClient ()
flushCacheToFile fp = do
    c <- get
    liftIO $ Yaml.encodeFile fp c
