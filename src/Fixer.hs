module Fixer
    ( fixer
    ) where

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client
import System.Exit
import Text.Read
import Web.HttpApiData

import Fixer.Client
import Fixer.Types

fixer :: IO ()
fixer = do
    res <- autoRunFixerClient $ getLatest Nothing Nothing
    case res of
        Left err -> die $ show err
        Right v -> print v
