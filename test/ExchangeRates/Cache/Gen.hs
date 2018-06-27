{-# OPTIONS_GHC -fno-warn-orphans #-}

module ExchangeRates.Cache.Gen where

import TestImport

import ExchangeRates.Types.Gen ()

import ExchangeRates.Cache

instance GenUnchecked ExchangeRatesCache

instance GenValid ExchangeRatesCache where
    genValid = ExchangeRatesCache <$> genValid <*> genValid

instance GenUnchecked ExchangeRatesCacheResult

instance GenValid ExchangeRatesCacheResult

instance GenUnchecked RateCache

instance GenValid RateCache where
    genValid = (RateCache <$> genValid) `suchThat` isValid
