{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The raw API
module Fixer.API
    ( FixerAPI
    , GetLatest
    , GetAtDate
    , fixerAPI
    , getLatest
    , getAtDate
    ) where

import Data.Proxy
import Data.Time
import Servant.API
import Servant.Client

import Fixer.Types

-- | A 'Proxy' for 'FixerAPI'
fixerAPI :: Proxy FixerAPI
fixerAPI = Proxy

-- | The full API at api.fixer.io
type FixerAPI = GetLatest :<|> GetAtDate

-- | Get latest rates
--
-- @/latest?base=\<base\>&symbols=\<symbols\>@
type GetLatest
     = "latest" :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

-- | Get rates on a given date
--
-- @/\<date\>?base=\<base\>&symbols=\<symbols\>@
type GetAtDate
     = Capture "date" Day :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

-- | The client function for 'GetLatest'
getLatest :: Maybe Currency -> Maybe Symbols -> ClientM Rates
-- | The client function for 'GetAtDate'
getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> ClientM Rates
getLatest :<|> getAtDate = client fixerAPI
