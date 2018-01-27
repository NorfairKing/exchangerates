{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Fixer.Client
    ( autoRunFixerClient
    , defaultConfig
    , runFixerClient
    , FClient
    , getLatest
    , getAtDate
    , RatesResult(..)
    , withFileCache
    , readCacheFromFileIfExists
    , flushCacheToFile
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Data.Validity
import qualified Data.Yaml as Yaml
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
import System.Directory (doesFileExist)

import qualified Fixer.API as Raw
import Fixer.Cache
import Fixer.Types

newtype FClient a = FClient
    { runFClient :: ReaderT FEnv ClientM a
    } deriving (Functor, Applicative, Monad, MonadReader FEnv, MonadIO)

data FEnv = FEnv
    { fEnvConfig :: Config
    , fEnvLastFetch :: TVar (Maybe UTCTime)
    , fEnvCache :: TVar FixerCache
    } deriving (Generic)

newtype Config = Config
    { confRateDelay :: Int -- ^ Microseconds
    } deriving (Show, Eq, Generic)

-- | Run a 'FClient' action and figure out the 'ClientEnv' and 'FixerCache'
-- arguments automatically.
--
-- This is probably the function you want to use
autoRunFixerClient :: FClient a -> IO (Either ServantError a)
autoRunFixerClient func = do
    man <- newManager defaultManagerSettings
    burl <- parseBaseUrl "http://api.fixer.io/"
    env <- makeEnv
    runFixerClient env (ClientEnv man burl) func

makeEnv :: IO FEnv
makeEnv = do
    lastFetchVar <- newTVarIO Nothing
    cacheVar <- newTVarIO emptyFixerCache
    pure
        FEnv
        { fEnvConfig = defaultConfig
        , fEnvLastFetch = lastFetchVar
        , fEnvCache = cacheVar
        }

defaultConfig :: Config
defaultConfig =
    Config
    { confRateDelay = 1 * microsecondsPerSecond -- One second
    }

-- | Run a 'FClient' action with full control over the inputs.
runFixerClient :: FEnv -> ClientEnv -> FClient a -> IO (Either ServantError a)
runFixerClient env ce func = runClientM (runReaderT (runFClient func) env) ce

-- | Get the latest rates.
--
-- Note that this function fetches the latest rates, but that does not mean
-- that the latest symbols appeared on the current date. However, there is no
-- way to predict what date the last rates appeared on, so we still look in the
-- cache at the current date.  For maximum cache hits, use 'getAtDate' and only
-- look at the past beyond the last three days.
getLatest :: Maybe Currency -> Maybe Symbols -> FClient RatesResult
getLatest mc ms = do
    date <- liftIO $ utctDay <$> getCurrentTime
    withCacheAndRate date mc ms $ Raw.getLatest mc ms

-- | Get the rates at a specific date.
getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> FClient RatesResult
getAtDate date mc ms = withCacheAndRate date mc ms $ Raw.getAtDate date mc ms

microsecondsPerSecond :: Int
microsecondsPerSecond = 1000 * 1000

data RatesResult
    = DateNotInPast
    | RateDoesNotExist
    | RatesFound Rates
    deriving (Show, Eq, Generic)

instance Validity RatesResult

withCacheAndRate ::
       Day
    -> Maybe Currency
    -> Maybe Symbols
    -> ClientM Rates
    -> FClient RatesResult
withCacheAndRate date mc ms func = do
    let base = fromMaybe defaultBaseCurrency mc
    let symbols = fromMaybe (allSymbolsExcept base) ms
    cacheVar <- asks fEnvCache
    now <- liftIO $ getCurrentTime
    let nowDate = utctDay now
    c <- liftIO $ readTVarIO cacheVar
    case lookupRates nowDate date base symbols c of
        NotInCache -> do
            rates <- rateLimit $ func
            liftIO $
                atomically $
                    modifyTVar' cacheVar (insertRates nowDate rates)
            pure $
                if ratesDate rates == date
                    then RatesFound rates
                    else if ratesDate rates >= nowDate
                             then DateNotInPast
                             else RateDoesNotExist
        CacheDateNotInPast -> pure DateNotInPast
        WillNeverExist -> pure RateDoesNotExist
        InCache r -> pure $ RatesFound r

rateLimit :: ClientM a -> FClient a
rateLimit func = do
    now <- liftIO $ getCurrentTime
    lastFetchVar <- asks fEnvLastFetch
    mLastFetch <- liftIO $ readTVarIO lastFetchVar
    delayTime <- asks $ confRateDelay . fEnvConfig
    let timeToWait =
            max 0 $
            case mLastFetch of
                Nothing -> 0
                Just lastFetch ->
                    let timeSinceLastFetch = diffUTCTime now lastFetch
                        timeSinceLastFetchMicro =
                            round $
                            (realToFrac timeSinceLastFetch *
                             fromIntegral microsecondsPerSecond :: Double)
                    in delayTime - timeSinceLastFetchMicro
            -- Wait
    liftIO $ threadDelay timeToWait
            -- Make the request
    res <- FClient $ lift func
    after <- liftIO getCurrentTime
    liftIO $ atomically $ writeTVar lastFetchVar $ Just after
    pure res

-- | Declare that we want to use the given file as a persistent cache.
--
-- Note that 'FClient' will still use a per-run cache if this function is not used.
-- This function only makes sure that the cache is persistent accross runs.
--
-- > withFileCache path func = do
-- >    readCacheFromFileIfExists path
-- >    r <- func
-- >    flushCacheToFile path
-- >    pure r
withFileCache :: FilePath -> FClient a -> FClient a
withFileCache path func = do
    readCacheFromFileIfExists path
    r <- func
    flushCacheToFile path
    pure r

-- | Read a persistent cache from the given file if that file exists.
readCacheFromFileIfExists :: FilePath -> FClient ()
readCacheFromFileIfExists fp = do
    fe <- liftIO $ doesFileExist fp
    mfc <-
        if fe
            then liftIO $ Yaml.decodeFile fp
            else pure Nothing
    case mfc of
        Nothing -> pure ()
        Just fc -> do
            cacheVar <- asks fEnvCache
            liftIO $ atomically $ writeTVar cacheVar fc

-- | Flush the currently gathered cache to the given file.
flushCacheToFile :: FilePath -> FClient ()
flushCacheToFile fp = do
    cacheVar <- asks fEnvCache
    liftIO $ do
        cache <- readTVarIO cacheVar
        Yaml.encodeFile fp cache
