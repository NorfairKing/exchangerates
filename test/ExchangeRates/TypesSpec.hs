{-# LANGUAGE TypeApplications #-}

module ExchangeRates.TypesSpec
    ( spec
    ) where

import TestImport

import Data.Ratio

import ExchangeRates.Types

import ExchangeRates.Types.Gen ()

spec :: Spec
spec = do
    eqSpec @Rate
    genValidSpec @Rate
    jsonSpecOnValid @Rate
    describe "Rate" $ do
        describe "oneRate" $ it "is a valid rate" $ shouldBeValid oneRate
        specify "0 % x is an invalid rate" $
            forAllValid $ \n -> Rate (0 % (n + 1)) `shouldNotSatisfy` isValid
        describe "normaliseRate" $
            it "produces valid rates" $ producesValidsOnValids normaliseRate
        describe "mulRate" $
            it "produces valid rates" $ producesValidsOnValids2 mulRate
        describe "divRate" $
            it "produces valid rates" $ producesValidsOnValids2 divRate
    eqSpec @Currency
    genValidSpec @Currency
    jsonSpecOnValid @Currency
    eqSpec @Symbols
    genValidSpec @Symbols
    eqSpec @Rates
    genValidSpec @Rates
