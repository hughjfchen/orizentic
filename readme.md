# Orizentic

Orizentic provides a library that streamlines token-based authentication and a CLI tool for maintaining the database.

## Credit

The name is a contraction of Auth(oriz)ation/Auth(entic)ation, and credit goes to [Daria Phoebe Brashear](https://github.com/dariaphoebe).

The original idea has been debated online for many years, but the push to make this useful comes from [Aria Stewart](https://github.com/aredridel).

## Tokens

Tokens are simple [JWTs](https://jwt.io/). This library simplifies the process by easily generating and checking JWTs that have only an issuer, an optional time-to-live, a resource name, a username, and a list of permissions. A typical resulting JWT would look like this:

    { iss = Savanni
    , sub = health
    , aud = "Savanni Desktop"
    , exp = null
    , nbf = null
    , iat = 1499650083
    , jti = 9d57a8d8-d11e-43b2-a4d6-7b82ad043994
    , unregisteredClaims = { perms: [ "read", "write" ] }
    }

The `issuer` and `audience` (or username) are almost entirely for human readability. In this instance, I issued a token that was intended to be used on my desktop system.

The `subject` in this case is synonymous with Resource and is a name for the resource for which access is being granted. Permissions are a simple list of freeform strings. Both of these are flexible within your application and your authorization checks will use them to verify that the token can be used for the specified purpose.

## Usage

A variety of workflows appear in the test code, however this is a workflow that I found to be typical in production code.

Start with creating the context:

```
claimsLst <- loadClaims "authDbPath"
ctx <- newOrizenticCtx ("JWT secret" :: Web.JWT.Secret) []
```

Your application monad will need to implement the `HasOrizenticCtx` type class, or you will need to call all of the orizentic functions with a reader monad.

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

