# simplebank-haskell

A very simple client to the very simple (and undocumented) simple.com API.

## Examples

```haskell
λ> s <- getSession (Configuration myUsername myPassword)
s :: Session
λ> balances s
Just (Balances {bills = 0, deposits = 0, goals = XXXXXX, pending = 0, safeToSpend = XXXXXXX, total = XXXXXXXXX}
it :: Maybe Balances
```

## License

BSD-2. :)
