{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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

fixerAPI :: Proxy FixerAPI
fixerAPI = Proxy

type FixerAPI = GetLatest :<|> GetAtDate

-- | @/latest?base=\<base\>&symbols=\<symbols\>@
type GetLatest
     = "latest" :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

-- | @/\<date\>?base=\<base\>&symbols=\<symbols\>@
type GetAtDate
     = Capture "date" Day :> QueryParam "base" Currency :> QueryParam "symbols" Symbols :> Get '[ JSON] Rates

getLatest :: Maybe Currency -> Maybe Symbols -> ClientM Rates
getAtDate :: Day -> Maybe Currency -> Maybe Symbols -> ClientM Rates
getLatest :<|> getAtDate = client fixerAPI
