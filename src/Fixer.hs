module Fixer
    ( fixer
    ) where

import System.Exit

import Fixer.Client

fixer :: IO ()
fixer = do
    res <- autoRunFixerClient $ getLatest Nothing Nothing
    case res of
        Left err -> die $ show err
        Right v -> print v
