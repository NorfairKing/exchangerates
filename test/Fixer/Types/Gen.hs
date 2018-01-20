{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixer.Types.Gen where

import TestImport

import Fixer.Types

instance GenUnchecked Currency

instance GenValid Currency

instance GenUnchecked Symbols

instance GenValid Symbols

instance GenUnchecked Rates

instance GenValid Rates
