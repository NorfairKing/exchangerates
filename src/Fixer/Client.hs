{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fixer.Client
    ( autoRunFixerClient
    , runFixerClient
    , FClient
    , getLatest
    , getAtDate
    , flushCacheToFile
    ) where

import Control.Monad.Reader
import Data.Time
import qualified Data.Yaml as Yaml
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client

import qualified Fixer.API as Raw
import Fixer.Cache
import Fixer.Types

newtype FClient a = FClient
    { runFClient :: ReaderT FixerCache ClientM a
    } deriving (Functor, Applicative, Monad, MonadReader FixerCache, MonadIO)

autoRunFixerClient :: FClient a -> IO (Either ServantError a)
autoRunFixerClient func = do
    man <- newManager defaultManagerSettings
    burl <- parseBaseUrl "http://api.fixer.io/"
    runFixerClient emptyFixerCache (ClientEnv man burl) func

runFixerClient ::
       FixerCache -> ClientEnv -> FClient a -> IO (Either ServantError a)
runFixerClient fc ce func = runClientM (runReaderT (runFClient func) fc) ce

getLatest :: Maybe Currency -> Maybe Symbols -> FClient Rates
getLatest mc ms = FClient $ lift $ Raw.getLatest mc ms

getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> FClient Rates
getAtDate d mc ms = FClient $ lift $ Raw.getAtDate d mc ms

flushCacheToFile :: FilePath -> FClient ()
flushCacheToFile fp = do
    c <- asks unFixerCache
    liftIO $ Yaml.encodeFile fp c
