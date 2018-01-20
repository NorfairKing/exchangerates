{-# LANGUAGE DeriveGeneric #-}

module Fixer.Cache
    ( FixerCache(..)
    , emptyFixerCache
    , insertRatesInCache
    , lookupRatesInCache
    -- Helpers
    , convertToBaseWithRate
    , rawInsertInCache
    , rawLookupInCache
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map (Map)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)

import Fixer.Types

newtype FixerCache = FixerCache
    { unFixerCache :: Map Day (Map Currency (Map Currency Rate))
    } deriving (Show, Eq, Generic)

instance Validity FixerCache

emptyFixerCache :: FixerCache
emptyFixerCache = FixerCache M.empty

rawInsertInCache ::
       Day -> Currency -> Currency -> Rate -> FixerCache -> FixerCache
rawInsertInCache d from to rate (FixerCache fc) = FixerCache $ M.alter go1 d fc
  where
    go1 :: Maybe (Map Currency (Map Currency Rate))
        -> Maybe (Map Currency (Map Currency Rate))
    go1 Nothing = Just $ M.singleton from $ M.singleton to rate
    go1 (Just c1) = Just $ M.alter go2 from c1
    go2 :: Maybe (Map Currency Rate) -> Maybe (Map Currency Rate)
    go2 Nothing = Just $ M.singleton to rate
    go2 (Just c2) = Just $ M.insert to rate c2

rawLookupInCache :: Day -> Currency -> Currency -> FixerCache -> Maybe Rate
rawLookupInCache d from to (FixerCache fc) =
    M.lookup d fc >>= M.lookup from >>= M.lookup to

defaultBaseCurrency :: Currency
defaultBaseCurrency = EUR

insertRatesInCache :: Rates -> FixerCache -> FixerCache
insertRatesInCache rs fc =
    if ratesBase rs == defaultBaseCurrency
        then insertRatesAtDefaultBase rs
            -- If we're not already using the base, then we need to see if we can figure out how many
            -- of this base we can get for the default base
            -- We can figure this out in two ways:
            -- 1 if the default base is in the rates
        else case M.lookup defaultBaseCurrency $ ratesRates rs of
                 Just r ->
                     insertRatesAtDefaultBase $
                     convertToBaseWithRate defaultBaseCurrency r rs
                 Nothing
                    -- or
                    -- 2 if the default base is in the cache
                  ->
                     case rawLookupInCache
                              (ratesDate rs)
                              (ratesBase rs)
                              defaultBaseCurrency
                              fc of
                         Just r ->
                             insertRatesAtDefaultBase $
                             convertToBaseWithRate defaultBaseCurrency r rs
                         Nothing
                            -- If we find neither, then we just save in the cache as-is
                          -> insertRatesAt (ratesBase rs) rs
  where
    insertRatesAtDefaultBase :: Rates -> FixerCache
    insertRatesAtDefaultBase = insertRatesAt defaultBaseCurrency
    insertRatesAt :: Currency -> Rates -> FixerCache
    insertRatesAt base = M.foldlWithKey (go base) fc . ratesRates
    go :: Currency -> FixerCache -> Currency -> Rate -> FixerCache
    go base fc_ c r = rawInsertInCache (ratesDate rs) base c r fc_

-- If the exact rates are already in the exact right spot, look them up.
lookupRatesInCache :: Day -> Currency -> Symbols -> FixerCache -> Maybe Rates
lookupRatesInCache date base (Symbols nec) (FixerCache fc) =
    Rates base date <$> go
  where
    go :: Maybe (Map Currency Rate)
    go = do
        dm <- M.lookup date fc
        bm <- M.lookup base dm
        (M.fromList . NE.toList) <$> mapM (\to -> (,) to <$> M.lookup to bm) nec

-- In the map, we have the info that
-- for 1 base currency, you get s of the currency in the key.
--
-- If we now say that for 1 of the old base currency, you can get
-- r of the new base currency
--
-- This rate means for one of the new base currency, you can get s / r of
-- the currency in the key.
convertToBaseWithRate :: Currency -> Rate -> Rates -> Rates
convertToBaseWithRate new rate rs =
    if ratesBase rs == new
        then rs
        else rs {ratesBase = new, ratesRates = newRates}
  where
    newRates =
        M.map (`divRate` rate) . withOldBase . withoutNewBase $ ratesRates rs
    withOldBase = M.insert (ratesBase rs) oneRate
    withoutNewBase = M.delete new
