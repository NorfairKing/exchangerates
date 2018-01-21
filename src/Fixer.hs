module Fixer
    ( fixer
    -- * Top-level API
    , FClient
    , autoRunFixerClient
    , runFixerClient
    , getLatest
    , getAtDate
    , readCacheFromFileIfExists
    , flushCacheToFile
    -- * Types
    , Currency(..)
    , Symbols(..)
    , Rates(..)
    -- * Re-exports
    -- ** NonEmpty
    , NonEmpty(..)
    -- ** Date
    , Day
    , fromGregorian
    ) where

import System.Exit

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import Fixer.Client
import Fixer.Types

fixer :: IO ()
fixer = do
    res <-
        autoRunFixerClient $ do
            let file = "/tmp/fixer.cache"
            readCacheFromFileIfExists file
            rates <- getAtDate (fromGregorian 2018 01 19) Nothing Nothing
            flushCacheToFile file
            pure rates
    case res of
        Left err -> die $ show err
        Right v -> print v
