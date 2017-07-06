{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module LuminescentDreams.Capabilities where

import Prelude  ( Bool(..), Either(..), Eq(..)
                , ($), (.)
                , fromRational, toRational
                , undefined
                )

import           Control.Applicative        ((<$>), pure)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader(..))
import           Data.IORef
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (Maybe(..), maybe)
import           Data.Text
import           Data.Time                  (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.UUID                  (toText)
import           System.Random              (randomIO)
import           Web.JWT

{- What is a token store? Is it anything more than a list of currently valid tokens? -}

newtype ResourceName = ResourceName Text
newtype Permissions = Permissions [Text]
newtype Issuer = Issuer Text
newtype TTL = TTL NominalDiffTime
newtype Username = Username Text

instance Eq (JWT VerifiedJWT) where
    j1 == j2 = claims j1 == claims j2

newtype TokenStore = TokenStore (IORef [JWTClaimsSet])

data CapabilityCtx = CapabilityCtx Secret TokenStore

newCapabilityContext :: MonadIO m => Secret -> m CapabilityCtx
newCapabilityContext secret = do
    st <- liftIO $ TokenStore <$> newIORef []
    pure $ CapabilityCtx secret st

class HasCapabilityCtx ctx where
    hasCapabilityCtx :: ctx -> CapabilityCtx

type TokenM m r = (MonadIO m, MonadReader r m, HasCapabilityCtx r)

validateToken :: TokenM m r => JWT UnverifiedJWT -> m (Maybe (JWT VerifiedJWT))
validateToken = undefined

checkAuthorizations :: (ResourceName -> Permissions -> Bool) -> JWT VerifiedJWT -> Bool
checkAuthorizations = undefined

createToken :: TokenM m r => Issuer -> TTL -> ResourceName -> Username -> Permissions -> m JWTClaimsSet
createToken (Issuer issuer) (TTL ttl) (ResourceName resourceName) (Username name) (Permissions perms) = do
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
                           , unregisteredClaims = M.empty
                           }
    liftIO $ modifyIORef store ((:) tok)
    pure tok


revokeToken :: TokenM m r => JWTClaimsSet -> m ()
revokeToken tok = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ modifyIORef store (L.delete tok)
    pure ()

listTokens :: TokenM m r => m [JWTClaimsSet]
listTokens = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ readIORef store

