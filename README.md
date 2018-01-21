# Fixer, a Haskell client for http://fixer.io/

Example usage:

``` Haskell
main :: IO
main = do
    rates <-
        autoRunFixerClient $
          withFileCache "/tmp/fixer.cache" $
            getLatest (Just EUR) Nothing
    case rates of
        Left err -> die $ show err
        Right v -> print v
```
