-- | The top-level API for the Haskell ExchangeRates client.
--
-- There is an example usage in the 'README' file.
module ExchangeRates
    ( FClient
    , autoRunExchangeRatesClient
    , RatesResult(..)
    , getLatest
    , getAtDate
    , withFileCache
    -- * Types
    , Currency(..)
    , Symbols(..)
    , Rate
    , Rates(..)
    , oneRate
    , mulRate
    , divRate
    , rateToDouble
    -- * Re-exports
    -- ** NonEmpty
    , NonEmpty(..)
    -- ** Date
    , Day
    , fromGregorian
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import ExchangeRates.Client
import ExchangeRates.Types
