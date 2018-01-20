module Fixer.CacheSpec
    ( spec
    ) where

import TestImport

import Fixer.Cache

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
                forAllValid $ \(c1, c2) ->
                    forAllValid $ \(r, fc) ->
                        let fc' = rawInsertInCache d c1 c2 r fc
                        in rawLookupInCache d c1 c2 fc' `shouldBe` Just r
