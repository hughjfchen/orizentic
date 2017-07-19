{-| Core functions for Orizentic
 -
 - The most conceptually confusing part of this is the relationship between the a Token, an unverified JWT, a verified JWT, and a claims set.
 -
 - A Token is a broad concept, and there is no particular data structure in this system that corresponds.
 -
 - Colloquially, however, a Token is synoymous with a JWT. JWTs can be either unverified or verified. An unverified JWT is anything that has been recieved from any outside source but whose signature has not yet been checked. a verified JWT has had the signature checked, at least as `Web.JWT` implements it. In this application, however, the process of checking the JWT signature also involves checking that the JWT is known to the application and that it has not expired.
 -
 - A ClaimsSet is an attribute of a JWT that is available for reading whether the JWT has been signed or whether the signature is valid. This describes all of the interesting parts of what the token is good for and who it should belong to.
 -
 - JWTs and their signatures typically get encoded into a Base64 format for transmission across the internet. `Web.JWT` provides the `encodeSigned` and `decode` functions to handle this process. This library provides `encodeToken` to do the conversion with a somewhat nicer API.
 -
 - Many operations in this library involve creating and managing claims sets. Additional functions are utilities for converting those claims sets both to and from JWT and fully encoded formats.
 - -}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module LuminescentDreams.Orizentic (
      ResourceName(..), Permissions(..), Issuer(..), TTL(..), Username(..)
    , OrizenticCtx(..), HasOrizenticCtx(..)
    , newOrizenticCtx
    , validateToken, checkAuthorizations
    , createClaims, revokeClaims, revokeByUUID, replaceClaims
    , listClaims, findClaims, encodeClaims, hasPermission, permissions
    ) where

import Prelude  ( Bool(..), Either(..), Eq(..), Show(..)
                , ($), (.), (<), (>>=)
                , filter, fmap, id
                , error
                )

import           Control.Applicative        ((<$>), pure)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader(..))
import           Data.Aeson                 (ToJSON(..), Result(..), fromJSON)
import           Data.IORef                 (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (Maybe(..), listToMaybe)
import           Data.Text                  (Text)
import           Data.Time                  (NominalDiffTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.UUID                  (toText)
import           System.Random              (randomIO)
import           Web.JWT


-- | ResourceName is application-defined for however the resources in the application should be named
newtype ResourceName = ResourceName Text deriving (Eq, Show)

-- | Permissions are application-defined descriptions of what can be done with the named resource
newtype Permissions = Permissions [Text] deriving Show

-- | Issuers are typically informative, but should generally describe who or what created the token
newtype Issuer = Issuer Text deriving Show

-- | Time to live is the number of seconds until a token expires
newtype TTL = TTL NominalDiffTime deriving Show

-- | Username, or Audience in JWT terms, should describe who or what is supposed to be using this token
newtype Username = Username Text deriving Show

instance Eq (JWT VerifiedJWT) where
    j1 == j2 = claims j1 == claims j2

newtype ClaimsStore = ClaimsStore (IORef [JWTClaimsSet])

data OrizenticCtx = OrizenticCtx Secret ClaimsStore

-- data CapDb = CapDb [JWTClaimsSet]
-- 
-- instance ToJSON CapDb where
--     toJSON (CapDb claims) = toJSON claims
-- 
-- instance FromJSON CapDb where
--     parseJSON lst = CapDb <$> parseJSON lst

class HasOrizenticCtx ctx where
    hasOrizenticCtx :: ctx -> OrizenticCtx

instance HasOrizenticCtx OrizenticCtx where
    hasOrizenticCtx = id

type OrizenticM m r = (MonadIO m, MonadReader r m, HasOrizenticCtx r)

{- | Create a new `OrizenticCtx` that will use a particular JWT secret and set of claims. --}
newOrizenticCtx :: MonadIO m => Secret -> [JWTClaimsSet] -> m OrizenticCtx
newOrizenticCtx s initialClaims = do
    st <- liftIO $ ClaimsStore <$> newIORef initialClaims
    pure $ OrizenticCtx s st

{- | Validate a token by checking its signature, that it is not expired, and that it is still present in the database. Return Nothing if any check fails, but return a verified JWT if it all succeeds. This function requires IO because it checks both the current database state and the current time. -}
validateToken :: OrizenticM m r => JWT UnverifiedJWT -> m (Maybe (JWT VerifiedJWT))
validateToken jwt = do
    now <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
    (OrizenticCtx s _) <- hasOrizenticCtx <$> ask
    case verify s jwt of
        Nothing -> pure Nothing
        Just vjwt -> if isExpired now (claims jwt)
            then pure Nothing
            else do lst <- listClaims
                    if claims jwt `L.elem` lst
                        then pure $ Just vjwt
                        else pure Nothing
    where
    isExpired now claimsSet =
        case secondsSinceEpoch <$> exp claimsSet of
            Nothing -> False
            Just expiration -> expiration < now


{- | Given a verified JWT, pass the resource name and permissions to a user-defined function. The function should return true if the caller should be granted access to the resource and falls, otherwise. That result will be passed back to the caller. -}
checkAuthorizations :: (ResourceName -> Permissions -> Bool) -> JWT VerifiedJWT -> Bool
checkAuthorizations fn token = 
    let claimsSet = claims token
        rn = ResourceName . stringOrURIToText <$> sub claimsSet
    in case rn of
        Nothing -> False
        Just rn_ -> fn rn_ (permissions claimsSet)


{- | Create a new JWTClaimsSet. This will create the claims (a tedious process with JWT) and add it to the database. It will also calculate and set the expiration time if a TTL is provided. -}
createClaims :: OrizenticM m r => Issuer -> Maybe TTL -> ResourceName -> Username -> Permissions -> m JWTClaimsSet
createClaims (Issuer issuer) ttl (ResourceName resourceName) (Username name) (Permissions perms) =
    let ttl_ = (\(TTL val) -> val) <$> ttl
    in do
    (OrizenticCtx _ (ClaimsStore store)) <- hasOrizenticCtx <$> ask
    now <- liftIO getCurrentTime
    uuid <- liftIO randomIO
    let tok = JWTClaimsSet { iss = stringOrURI issuer
                           , sub = stringOrURI resourceName
                           , aud = Left <$> stringOrURI name
                           , exp = (utcTimeToPOSIXSeconds . (`addUTCTime` now) <$> ttl_) >>= numericDate
                           , nbf = Nothing
                           , iat = numericDate $ utcTimeToPOSIXSeconds now
                           , jti = stringOrURI $ toText uuid
                           , unregisteredClaims = M.fromList [("perms", toJSON perms)]
                           }
    liftIO $ modifyIORef store ((:) tok)
    pure tok


{- | Remove a claims set from the database so that all additional validation checks fail. -}
revokeClaims :: OrizenticM m r => JWTClaimsSet -> m ()
revokeClaims tok = do
    (OrizenticCtx _ (ClaimsStore store)) <- hasOrizenticCtx <$> ask
    liftIO $ modifyIORef store (L.delete tok)
    pure ()


{- | Revoke a ClaimsSet given its UUID, which is set in the `jti` claim. -}
revokeByUUID :: OrizenticM m r => Text -> m ()
revokeByUUID uuid = do
    (OrizenticCtx _ (ClaimsStore store)) <- hasOrizenticCtx <$> ask
    liftIO $ modifyIORef store (filter (\c -> getUUID c /= Just uuid))
    where
    getUUID = fmap stringOrURIToText . jti
    

{- | Replace the entire list of claims currently in memory. This is typically used when reloading a claims set from disk. -}
replaceClaims :: OrizenticM m r => [JWTClaimsSet] -> m ()
replaceClaims newClaims = do
    (OrizenticCtx _ (ClaimsStore store)) <- hasOrizenticCtx <$> ask
    liftIO $ writeIORef store newClaims


{- | Return all of the ClaimsSets currently in the database. -}
listClaims :: OrizenticM m r => m [JWTClaimsSet]
listClaims = do
    (OrizenticCtx _ (ClaimsStore store)) <- hasOrizenticCtx <$> ask
    liftIO $ readIORef store


{- | Find a ClaimsSet by UUID -}
findClaims :: OrizenticM m r => Text -> m (Maybe JWTClaimsSet)
findClaims uuid = do
    lst <- listClaims
    pure $ listToMaybe $ filter (\c -> getUUID c == Just uuid) lst
    where
    getUUID = fmap stringOrURIToText . jti

-- TODO: maybe this should become part of a utility library
-- saveDB :: TokenM m r => FilePath -> m ()
-- saveDB path = do
--     (OrizenticCtx s _) <- hasOrizenticCtx <$> ask
--     claims <- listClaims
--     liftIO $ writeFile path $ toStrict $ encode $ CapDb claims
-- 
-- 
-- reloadDB :: TokenM m r => FilePath -> m ()
-- reloadDB path = do
--     (OrizenticCtx s (ClaimsStore store)) <- hasOrizenticCtx <$> ask
--     db <- (eitherDecode . fromStrict) <$> (liftIO $ readFile path)
--     case db of
--         Left err -> undefined
--         Right (CapDb claims) -> liftIO $ writeIORef store claims


{- | Encode a ClaimsSet using this context's secret. -}
encodeClaims :: OrizenticM m r => JWTClaimsSet -> m Text
encodeClaims token = do
    (OrizenticCtx s _) <- hasOrizenticCtx <$> ask
    pure $ encodeSigned HS256 s token


hasPermission :: Permissions -> Text -> Bool
hasPermission (Permissions perms) p = p `L.elem` perms


permissions :: JWTClaimsSet -> Permissions
permissions claimsSet =
    case M.lookup "perms" $ unregisteredClaims claimsSet of
        Nothing -> Permissions []
        Just claimsPermissions -> case fromJSON claimsPermissions of
            Error err -> error $ show err
            Success s -> Permissions s

