{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixer.Cache.Gen where

import TestImport

import Fixer.Types.Gen ()

import Fixer.Cache

instance GenUnchecked FixerCache

instance GenValid FixerCache where
    genValid = FixerCache <$> genValid <*> genValid

instance GenUnchecked FixerCacheResult

instance GenValid FixerCacheResult

instance GenUnchecked RateCache

instance GenValid RateCache where
    genValid = (RateCache <$> genValid) `suchThat` isValid
