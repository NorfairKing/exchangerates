{-# LANGUAGE TypeApplications #-}

module Fixer.TypesSpec
    ( spec
    ) where

import TestImport

import Fixer.Types

import Fixer.Types.Gen ()

spec :: Spec
spec = do
    eqSpec @Currency
    genValidSpec @Currency
    jsonSpecOnValid @Currency
