{-# LANGUAGE TypeApplications #-}

module Fixer.CacheSpec
    ( spec
    ) where

import TestImport

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

import Fixer.Cache
import Fixer.Types

import Fixer.Cache.Gen ()
import Fixer.Types.Gen ()

spec :: Spec
spec = do
    eqSpecOnValid @FixerCache
    genValidSpec @FixerCache
    jsonSpecOnValid @FixerCache
    describe "rawInsertInCache" $
        it "produces valid caches if the currencies are distinct" $
        forAllValid $ \d ->
            forAllValid $ \c1 ->
                forAll (genValid `suchThat` (/= c1)) $ \c2 ->
                    forAllValid $ \r ->
                        producesValidsOnValids $ rawInsertInCache d c1 c2 r
    describe "rawLookupInCache" $ do
        it "produces valid rates" $
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
    describe "defaultBaseCurrency" $
        it "is valid" $ shouldBeValid defaultBaseCurrency
    describe "allSymbols" $
        it "is valid" $ producesValidsOnValids allSymbolsExcept
    describe "insertRatesInCache" $
        it "produces valid caches" $
        forAllValid $ \rates ->
            producesValidsOnValids $ insertRatesInCache rates
    describe "smartLookupRateInCache" $ do
        it "produces valid rates" $
            forAllValid $ \date ->
                forAllValid $ \from ->
                    forAllValid $ \to ->
                        producesValidsOnValids $
                        smartLookupRateInCache date from to
        it
            "finds the same as rawLookupInCache if rawLookupInCache finds a result" $
            forAllValid $ \date ->
                forAllValid $ \from ->
                    forAllValid $ \to ->
                        equivalentWhenSucceedOnValid
                            (rawLookupInCache date from to)
                            (smartLookupRateInCache date from to)
        it "succesfully lookups up a rate that is cached in reverse" $
            forAllValid $ \date ->
                forAllValid $ \from ->
                    forAllValid $ \to ->
                        forAllValid $ \rate ->
                            forAllValid $ \fc ->
                                unless (from == to) $ do
                                    let fc' =
                                            smartInsertInCache
                                                date
                                                from
                                                to
                                                rate
                                                fc
                                    shouldBeValid fc'
                                    smartLookupRateInCache date to from fc' `shouldBe`
                                        (Just $ divRate oneRate rate)
    describe "lookupRatesInCache" $ do
        it "produces valid caches" $
            forAllValid $ \date ->
                forAllValid $ \base ->
                    forAllValid $ \symbols ->
                        producesValidsOnValids $
                        lookupRatesInCache date base symbols
        it "works for a cache with a single rate" $
            forAllValid $ \date ->
                forAllValid $ \base ->
                    forAll (genValid `suchThat` (/= base)) $ \symbol ->
                        forAllValid $ \rate ->
                            let cache =
                                    FixerCache
                                    { unFixerCache =
                                          M.fromList
                                              [ ( date
                                                , M.fromList
                                                      [ ( base
                                                        , M.fromList
                                                              [(symbol, rate)])
                                                      ])
                                              ]
                                    }
                                rates =
                                    Rates
                                    { ratesBase = base
                                    , ratesDate = date
                                    , ratesRates = M.fromList [(symbol, rate)]
                                    }
                            in lookupRatesInCache
                                   date
                                   base
                                   (Symbols $ symbol :| [])
                                   cache `shouldBe`
                               Just rates
        it
            "finds the rates that were just added with insertRatesInCache when adding at the default base currency" $
            forAllValid $ \rates' ->
                forAllValid $ \fc ->
                    let rates =
                            rates'
                            { ratesBase = defaultBaseCurrency
                            , ratesRates =
                                  M.delete defaultBaseCurrency $
                                  ratesRates rates'
                            }
                        fc' = insertRatesInCache rates fc
                    in case NE.nonEmpty (M.keys $ ratesRates rates) of
                           Nothing -> pure () -- Fine
                           Just symbols ->
                               lookupRatesInCache
                                   (ratesDate rates)
                                   (ratesBase rates)
                                   (Symbols symbols)
                                   fc' `shouldBe`
                               Just rates
        it "finds the rates that were just added with insertRatesInCache" $
            forAllValid $ \rates ->
                forAllValid $ \fc ->
                    let fc' = insertRatesInCache rates fc
                    in case NE.nonEmpty (M.keys $ ratesRates rates) of
                           Nothing -> pure () -- Fine
                           Just symbols ->
                               lookupRatesInCache
                                   (ratesDate rates)
                                   (ratesBase rates)
                                   (Symbols symbols)
                                   fc' `shouldBe`
                               Just rates
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
