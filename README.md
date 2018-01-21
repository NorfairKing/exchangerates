# Fixer, a Haskell client for http://fixer.io/

## Automatic Transparent Caching

Because historical currency conversion rates do not change post-hoc,
we can cache them indefinitely.
Because of this, and because http://fixer.io/ is a free service,
we should cache the results of the API as much as possible.
This client automatically transparenly caches results so that the real
API is only called when absolutely necessary.

## Example usage:

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
