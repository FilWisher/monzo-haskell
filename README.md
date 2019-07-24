# monzo-haskell

Haskell bindings to the Monzo API.

 - `Web.Monzo` - high-level IO-based API calls
 - `Web.Monzo.Monad` - mtl-based API calls
 - `Web.Monzo.Auth` - IO-based calls for OAuth2 authorization

## Example:

```hs
{-# LANGUAGE OverloadedStrings #-}

import Monzo.Web
import Network.HTTP.Client.TLS

token :: AccessToken
token = "..."

main :: IO ()
main = do
    mgr <- newTlsManager
    eaccounts <- listAccounts mgr token
    case eaccounts of
        Left err -> print err
        Right accounts -> do
            case filter (not . accountClosed) account of
                [] -> print "No open accounts"
                (a:as) -> getBalance mgr token (accountId a) >>= print
```
