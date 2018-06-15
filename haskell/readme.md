# Haskell Usage

A variety of workflows appear in the test code, however this is a workflow that I found to be typical in production code.

Start with creating the context:

```
claimsLst <- loadClaims "authDbPath"
ctx <- newOrizenticCtx ("JWT secret" :: Web.JWT.Secret) []
```

Your application monad will need to implement the `HasOrizenticCtx` type class, or you will need to call all of the orizentic functions with a reader monad.

```
data AppContext -- Your application context here

instance HasOrizenticCtx AppContext where
    hasOrizenticCtx = ...
```

A typical authentication/verification function will look like this:

```
authenticate :: (HasOrizenticCtx m) => JWT UnverifiedJWT -> m False
authenticate token = do
    res <- validateToken token
    case res of
        Nothing -> False    -- The token is invalid. Perhaps not a token at all,
                            -- perhaps not signed with the key for this application,
                            -- or perhaps forged.
        Just jwt -> checkAuthorizations validatePermissions jwt
    where
    -- Validate permissions receives the resource name and a list of
    -- permissions. In this case, the "resource" is a name I designated to the
    -- entire app, and the only valid permissions are read+write. A more
    -- sophisticated app will want a validator with more resource names and more
    -- nuanced permissions.
    validatePermissions rn (Permissions perms) =
            (rn == ResourceName "health")
        &&  ("read"     `elem` perms)
        &&  ("write"    `elem` perms)
```

## CLI

`orizentic` is a command-line tool which manages a database stored to disk. The database is a plain text format containing JWT claims. It can be easily loaded with this function:

```
loadClaims :: FilePath -> IO (Either String [JWTClaimsSet])
loadClaims path =
    catch ((eitherDecode . fromStrict) <$> Data.ByteString.readFile path)
          (\e -> if isDoesNotExistError e
                    then pure $ Right []
                    else throw e)
```

The utility is provided as a convienence for simple applications with rare database changes. Production environments will likely wish to manage this within a more sophisticated application and may wish to use some other storage backend.

