# Orizentic

[![CircleCI](https://circleci.com/gh/luminescent-dreams/orizentic/tree/sol.svg?style=svg)](https://circleci.com/gh/luminescent-dreams/orizentic/tree/sol)

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

## Language support

This library and application are supported for Haskell, Go, and Rust. The token database is compatible across tools. See readme's in the language directory for usage information.

