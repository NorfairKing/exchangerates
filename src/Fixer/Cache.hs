{-# LANGUAGE DeriveGeneric #-}

module Fixer.Cache
    ( FixerCache(..)
    , emptyFixerCache
    , rawInsertInCache
    , rawLookupInCache
    ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)

import Fixer.Types

newtype FixerCache = FixerCache
    { unFixerCache :: Map Day (Map Currency (Map Currency Double))
    } deriving (Show, Eq, Generic)

instance Validity FixerCache

emptyFixerCache :: FixerCache
emptyFixerCache = FixerCache M.empty

rawInsertInCache ::
       Day -> Currency -> Currency -> Double -> FixerCache -> FixerCache
rawInsertInCache d from to rate (FixerCache fc) = FixerCache $ M.alter go1 d fc
  where
    go1 :: Maybe (Map Currency (Map Currency Double))
        -> Maybe (Map Currency (Map Currency Double))
    go1 Nothing = Just $ M.singleton from $ M.singleton to rate
    go1 (Just c1) = Just $ M.alter go2 from c1
    go2 :: Maybe (Map Currency Double) -> Maybe (Map Currency Double)
    go2 Nothing = Just $ M.singleton to rate
    go2 (Just c2) = Just $ M.insert to rate c2

rawLookupInCache :: Day -> Currency -> Currency -> FixerCache -> Maybe Double
rawLookupInCache d from to (FixerCache fc) =
    M.lookup d fc >>= M.lookup from >>= M.lookup to
