{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixer.Cache.Gen where

import TestImport

import Fixer.Types.Gen ()

import Fixer.Cache

instance GenUnchecked FixerCache

instance GenValid FixerCache
