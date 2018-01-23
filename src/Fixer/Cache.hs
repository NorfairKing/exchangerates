{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Fixer.Cache
    ( FixerCache(..)
    , emptyFixerCache
    , insertRatesInCache
    , lookupRatesInCache
    , smartInsertInCache
    , smartLookupRateInCache
    -- Defaults
    , defaultBaseCurrency
    , allSymbolsExcept
    -- Helpers
    , convertToBaseWithRate
    , rawInsertInCache
    , rawLookupInCache
    ) where

import Control.Monad
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Time
import Data.Validity
import GHC.Generics (Generic)

import Fixer.Types

-- | A cache for currency rates
--
-- This cache uses 'EUR' as the base currency, but will still cache
-- rates appropriately if rates with a different base currency are cached.
newtype FixerCache = FixerCache
    { unFixerCache :: Map Day (Map Currency (Map Currency Rate))
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity FixerCache where
    validate FixerCache {..} =
        mconcat
            [ unFixerCache <?!> "unFixerCache"
            , let go :: Map Currency (Map Currency Rate) -> Bool
                  go m =
                      not . or $
                      M.mapWithKey (\c m_ -> isJust (M.lookup c m_)) m
              in all go unFixerCache <?@>
                 "Does not contain conversions to from a currency to itself"
            ]
    isValid = isValidByValidating

-- | The empty Cache
emptyFixerCache :: FixerCache
emptyFixerCache = FixerCache M.empty

-- | Insert a rate into the cache as-is.
--
-- You probably want to be using 'insertRatesInCache' or 'smartInsertInCache' instead.
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

-- | Lookup a rate in the cache as-is.
--
-- You probably want to be using 'smartLookupRateInCache' instead.
rawLookupInCache :: Day -> Currency -> Currency -> FixerCache -> Maybe Rate
rawLookupInCache d from to (FixerCache fc) =
    M.lookup d fc >>= M.lookup from >>= M.lookup to

-- | The default base currency. Currently this is 'EUR'
defaultBaseCurrency :: Currency
defaultBaseCurrency = EUR

-- | The symbols to get by default, given a base currency.
allSymbolsExcept :: Currency -> Symbols
allSymbolsExcept base =
    Symbols $ NE.fromList $ filter (/= base) [minBound .. maxBound]

-- | Insert a result into the cache.
--
-- This is probably the function you want to use, it does all the smartness.
insertRatesInCache :: Rates -> FixerCache -> FixerCache
insertRatesInCache rs fc =
    if ratesBase rs == defaultBaseCurrency
        then insertRatesAsIs rs
            -- If we're not already using the base, then we need to see if we can figure out how many
            -- of this base we can get for the default base
            -- We can figure this out in two ways:
            -- 1 if the default base is in the rates
        else case M.lookup defaultBaseCurrency $ ratesRates rs of
                 Just r -> insertRatesAtOtherBase r rs
                 Nothing
                    -- or
                    -- 2 if the default base is in the cache
                  ->
                     case rawLookupInCache
                              (ratesDate rs)
                              (ratesBase rs)
                              defaultBaseCurrency
                              fc of
                         Just r -> insertRatesAtOtherBase r rs
                         Nothing
                            -- If we find neither, then we just save in the cache as-is
                          -> insertRatesAsIs rs
  where
    insertRatesAsIs :: Rates -> FixerCache
    insertRatesAsIs rates =
        M.foldlWithKey (go (ratesBase rates)) fc $ ratesRates rates
    insertRatesAtOtherBase :: Rate -> Rates -> FixerCache
    insertRatesAtOtherBase r =
        insertRatesAsIs . convertToBaseWithRate defaultBaseCurrency r
    go :: Currency -> FixerCache -> Currency -> Rate -> FixerCache
    go base fc_ c r = smartInsertInCache (ratesDate rs) base c r fc_

-- | Insert a rate in a cache, but don't insert it if the from and to currencies are the same.
smartInsertInCache ::
       Day -> Currency -> Currency -> Rate -> FixerCache -> FixerCache
smartInsertInCache date from to rate fc =
    if from == to
        then fc
        else rawInsertInCache date from to rate fc

-- | Look up multiple rates in a cache.
--
-- This function uses 'smartLookupRateInCache' for each requested symbol.
lookupRatesInCache :: Day -> Currency -> Symbols -> FixerCache -> Maybe Rates
lookupRatesInCache date base (Symbols nec) fc =
    Rates base date <$>
    (M.fromList <$>
     mapM
         (\to -> (,) to <$> smartLookupRateInCache date base to fc)
         (NE.filter (/= base) nec))

-- | Look up a rate in a cache.
--
-- This function will try to be smart about what it can find, but will
-- give up after one redirection.
smartLookupRateInCache ::
       Day -> Currency -> Currency -> FixerCache -> Maybe Rate
smartLookupRateInCache date from to fc@(FixerCache m) =
    if from == to
        then Just oneRate
        else case rawLookupInCache date from to fc of
                 Just r -> pure r
                 -- First try to look up at the correct base currency
                 -- If that works, return it.
                 -- Otherwise, try all the other bases at that day, and convert if necessary.
                 Nothing -> do
                     dm <- M.lookup date m
                     msum $
                         M.elems $
                         flip M.mapWithKey dm $ \newFrom nfm ->
                             lookupVia newFrom from to nfm

lookupVia :: Currency -> Currency -> Currency -> Map Currency Rate -> Maybe Rate
lookupVia newFrom from to nfm = do
    nfr <-
        if newFrom == from
            then Just oneRate
            else M.lookup from nfm
    -- This is the rate at which we can convert from newFrom to from
    -- for each 'from', you get '1/nfr' newFroms
    tr <-
        if newFrom == to
            then Just oneRate
            else M.lookup to nfm
    -- This is the rate at which we can convert from newFrom to to
    pure $ divRate tr nfr

-- | Convert a set of rates to another base currency with the given rate of the new base currency
-- with respect to the old base currency.
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
