module Fixer.CacheSpec
    ( spec
    ) where

import TestImport

import qualified Data.Map as M

import Fixer.Cache
import Fixer.Types

import Fixer.Cache.Gen ()
import Fixer.Types.Gen ()

spec :: Spec
spec = do
    describe "rawInsertInCache" $
        it "produces valid caches" $
        producesValidsOnValids3 (uncurry $ uncurry rawInsertInCache)
    describe "rawLookupInCache" $ do
        it "produces valid caches" $
            producesValidsOnValids3 (uncurry rawLookupInCache)
        it "finds what was just put there" $
            forAllValid $ \d ->
                forAllValid $ \c1 ->
                    forAllValid $ \c2 ->
                        forAllValid $ \r ->
                            forAllValid $ \fc ->
                                let fc' = rawInsertInCache d c1 c2 r fc
                                in rawLookupInCache d c1 c2 fc' `shouldBe`
                                   Just r
    describe "insertRatesInCache" $
        it "produces valid caches" $
        forAllValid $ \rates ->
            producesValidsOnValids $ insertRatesInCache rates
    describe "lookupRatesInCache" $
        it "produces valid caches" $
        forAllValid $ \date ->
            forAllValid $ \base ->
                forAllValid $ \symbols ->
                    producesValidsOnValids $
                    lookupRatesInCache date base symbols
    describe "convertToBaseWithRate" $ do
        it "produces valid caches" $
            forAllValid $ \base ->
                forAllValid $ \rate ->
                    producesValidsOnValids $ convertToBaseWithRate base rate
        it
            "converts the rates such that the new base has a 1-to-rate conversion with the old base" $
            forAllValid $ \base ->
                forAllValid $ \rate ->
                    forAllValid $ \rates ->
                        unless (base == ratesBase rates) $
                        let rates' =
                                rates
                                { ratesRates =
                                      M.insert base rate $ ratesRates rates
                                }
                            rates'' = convertToBaseWithRate base rate rates'
                        in M.lookup (ratesBase rates) (ratesRates rates'') `shouldBe`
                           Just (divRate oneRate rate)
