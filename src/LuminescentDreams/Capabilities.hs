{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module LuminescentDreams.Capabilities where

import Prelude  ( Bool(..), Either(..), Eq(..)
                , ($), (.), (<)
                , error, show
                )

import           Control.Applicative        ((<$>), pure)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader(..))
import           Data.Aeson                 (fromJSON, toJSON, Result(..))
import           Data.IORef                 (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (Maybe(..))
import           Data.Text                  (Text)
import           Data.Time                  (NominalDiffTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.UUID                  (toText)
import           System.Random              (randomIO)
import           Web.JWT

{- What is a token store? Is it anything more than a list of currently valid tokens? -}

newtype ResourceName = ResourceName Text deriving Eq
newtype Permissions = Permissions [Text]
newtype Issuer = Issuer Text
newtype TTL = TTL NominalDiffTime
newtype Username = Username Text

instance Eq (JWT VerifiedJWT) where
    j1 == j2 = claims j1 == claims j2

newtype TokenStore = TokenStore (IORef [JWTClaimsSet])

data CapabilityCtx = CapabilityCtx Secret TokenStore

newCapabilityContext :: MonadIO m => Secret -> m CapabilityCtx
newCapabilityContext s = do
    st <- liftIO $ TokenStore <$> newIORef []
    pure $ CapabilityCtx s st

class HasCapabilityCtx ctx where
    hasCapabilityCtx :: ctx -> CapabilityCtx

type TokenM m r = (MonadIO m, MonadReader r m, HasCapabilityCtx r)


validateToken :: TokenM m r => JWT UnverifiedJWT -> m (Maybe (JWT VerifiedJWT))
validateToken jwt = do
    now <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
    (CapabilityCtx s _) <- hasCapabilityCtx <$> ask
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


checkAuthorizations :: (ResourceName -> Permissions -> Bool) -> JWT VerifiedJWT -> Bool
checkAuthorizations fn token = 
    let claimsSet = claims token
        rn = ResourceName . stringOrURIToText <$> sub claimsSet
    in case rn of
        Nothing -> False
        Just rn_ -> fn rn_ (permissions $ claimsSet)


createClaims :: TokenM m r => Issuer -> TTL -> ResourceName -> Username -> Permissions -> m JWTClaimsSet
createClaims (Issuer issuer) (TTL ttl) (ResourceName resourceName) (Username name) (Permissions perms) = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    now <- liftIO getCurrentTime
    uuid <- liftIO randomIO
    let tok = JWTClaimsSet { iss = stringOrURI issuer
                           , sub = stringOrURI resourceName
                           , aud = Left <$> stringOrURI name
                           , exp = numericDate $ utcTimeToPOSIXSeconds $ addUTCTime ttl now
                           , nbf = Nothing
                           , iat = numericDate $ utcTimeToPOSIXSeconds now
                           , jti = stringOrURI $ toText uuid
                           , unregisteredClaims = M.fromList [("perms", toJSON perms)]
                           }
    liftIO $ modifyIORef store ((:) tok)
    pure tok


revokeClaims :: TokenM m r => JWTClaimsSet -> m ()
revokeClaims tok = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ modifyIORef store (L.delete tok)
    pure ()


listClaims :: TokenM m r => m [JWTClaimsSet]
listClaims = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ readIORef store


encodeToken :: TokenM m r => JWTClaimsSet -> m Text
encodeToken token = do
    (CapabilityCtx s _) <- hasCapabilityCtx <$> ask
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

--         perms = case (maybe (Error maybe (Permissions [])
--                       Permissions $ ((M.lookup "perms" $ unregisteredClaims claimsSet) >>= fromJSON)

