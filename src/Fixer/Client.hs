{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fixer.Client
    ( autoRunFixerClient
    , runFixerClient
    , FClient
    , getLatest
    , getAtDate
    , withFileCache
    , readCacheFromFileIfExists
    , flushCacheToFile
    ) where

import Control.Monad.State
import Data.Maybe
import Data.Time
import qualified Data.Yaml as Yaml
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
import System.Directory (doesFileExist)

import qualified Fixer.API as Raw
import Fixer.Cache
import Fixer.Types

newtype FClient a = FClient
    { runFClient :: StateT FixerCache ClientM a
    } deriving (Functor, Applicative, Monad, MonadState FixerCache, MonadIO)

-- | Run a 'FClient' action and figure out the 'ClientEnv' and 'FixerCache'
-- arguments automatically.
--
-- This is probably the function you want to use
autoRunFixerClient :: FClient a -> IO (Either ServantError a)
autoRunFixerClient func = do
    man <- newManager defaultManagerSettings
    burl <- parseBaseUrl "http://api.fixer.io/"
    runFixerClient emptyFixerCache (ClientEnv man burl) func

runFixerClient ::
       FixerCache -> ClientEnv -> FClient a -> IO (Either ServantError a)
runFixerClient fc ce func = runClientM (evalStateT (runFClient func) fc) ce

-- | Get the latest rates.
--
-- Note that this function fetches the latest rates, but that does not mean
-- that the latest symbols appeared on the current date. However, there is no
-- way to predict what date the last rates appeared on, so we still look in the
-- cache at the current date.  For maximum cache hits, use 'getAtDate' and only
-- look at the past beyond the last three days.
getLatest :: Maybe Currency -> Maybe Symbols -> FClient Rates
getLatest mc ms = do
    date <- liftIO $ utctDay <$> getCurrentTime
    withCache date mc ms $ Raw.getLatest mc ms

-- | Get the rates at a specific date.
getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> FClient Rates
getAtDate date mc ms = withCache date mc ms $ Raw.getAtDate date mc ms

withCache ::
       Day -> Maybe Currency -> Maybe Symbols -> ClientM Rates -> FClient Rates
withCache date mc ms func = do
    let base = fromMaybe defaultBaseCurrency mc
    let symbols = fromMaybe (allSymbolsExcept base) ms
    c <- get
    case lookupRatesInCache date base symbols c of
        Nothing ->
            FClient $ do
                rates <- lift func
                modify (insertRatesInCache rates)
                pure rates
        Just rates -> pure rates

withFileCache :: FilePath -> FClient a -> FClient a
withFileCache path func = do
    readCacheFromFileIfExists path
    r <- func
    flushCacheToFile path
    pure r

readCacheFromFileIfExists :: FilePath -> FClient ()
readCacheFromFileIfExists fp = do
    fe <- liftIO $ doesFileExist fp
    mfc <-
        if fe
            then liftIO $ Yaml.decodeFile fp
            else pure Nothing
    case mfc of
        Nothing -> pure ()
        Just fc -> put fc

flushCacheToFile :: FilePath -> FClient ()
flushCacheToFile fp = do
    c <- get
    liftIO $ Yaml.encodeFile fp c
