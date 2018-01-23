-- | The top-level API for the Haskell Fixer client.
--
-- There is an example usage in the 'README' file.
module Fixer
    ( FClient
    , autoRunFixerClient
    , getLatest
    , getAtDate
    , withFileCache
    -- * Types
    , Currency(..)
    , Symbols(..)
    , Rates(..)
    -- * Re-exports
    -- ** NonEmpty
    , NonEmpty(..)
    -- ** Date
    , Day
    , fromGregorian
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import Fixer.Client
import Fixer.Types
