module Fixer
    ( fixer
    , FClient
    , autoRunFixerClient
    , runFixerClient
    , getLatest
    , getAtDate
    , readCacheFromFile
    , flushCacheToFile
    ) where

import System.Exit

import Fixer.Client

fixer :: IO ()
fixer = do
    res <-
        autoRunFixerClient $ do
            let file = "/tmp/fixer.cache"
            readCacheFromFile file
            rates <- getLatest Nothing Nothing
            flushCacheToFile file
            pure rates
    case res of
        Left err -> die $ show err
        Right v -> print v
