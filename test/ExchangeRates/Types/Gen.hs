{-# OPTIONS_GHC -fno-warn-orphans #-}

module ExchangeRates.Types.Gen where

import TestImport

import Numeric.Natural

import ExchangeRates.Types

-- Until genvalidity has these.
instance GenUnchecked Natural where
    genUnchecked = fromInteger . abs <$> genValid -- Cannot even generate wrong ones
    shrinkUnchecked _ = []

instance GenValid Natural where
    genValid = fromInteger . abs <$> genValid

instance GenUnchecked Rate

instance GenValid Rate where
    genValid = (Rate <$> genValid) `suchThat` isValid

instance GenUnchecked Currency

instance GenValid Currency

instance GenUnchecked Symbols

instance GenValid Symbols where
    genValid = Symbols <$> genValid

instance GenUnchecked Rates

instance GenValid Rates where
    genValid = (Rates <$> genValid <*> genValid <*> genValid) `suchThat` isValid
