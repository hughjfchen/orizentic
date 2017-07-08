{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module LuminescentDreams.Capabilities where

import Prelude  ( Bool(..), Either(..), Eq(..), FilePath, Show(..)
                , ($), (.), (<), (>>=)
                , filter, fmap, id
                , error, undefined
                )

import           Control.Applicative        ((<$>), pure)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader(..), runReaderT)
import           Data.Aeson                 (FromJSON(..), ToJSON(..), Result(..), (.=), (.:), eitherDecode, encode, fromJSON, object)
import           Data.ByteString            (readFile, writeFile)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.IORef                 (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (Maybe(..))
import           Data.Text                  (Text)
import           Data.Time                  (NominalDiffTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.UUID                  (UUID, fromText, toText)
import           System.Random              (randomIO)
import           Web.JWT

{- What is a token store? Is it anything more than a list of currently valid tokens? -}

newtype ResourceName = ResourceName Text deriving (Eq, Show)
newtype Permissions = Permissions [Text] deriving Show
newtype Issuer = Issuer Text deriving Show
newtype TTL = TTL NominalDiffTime deriving Show
newtype Username = Username Text deriving Show

instance Eq (JWT VerifiedJWT) where
    j1 == j2 = claims j1 == claims j2

newtype TokenStore = TokenStore (IORef [JWTClaimsSet])

data CapabilityCtx = CapabilityCtx Secret TokenStore

-- data CapDb = CapDb [JWTClaimsSet]
-- 
-- instance ToJSON CapDb where
--     toJSON (CapDb claims) = toJSON claims
-- 
-- instance FromJSON CapDb where
--     parseJSON lst = CapDb <$> parseJSON lst

class HasCapabilityCtx ctx where
    hasCapabilityCtx :: ctx -> CapabilityCtx

instance HasCapabilityCtx CapabilityCtx where
    hasCapabilityCtx = id

newCapabilityContext :: MonadIO m => Secret -> [JWTClaimsSet] -> m CapabilityCtx
newCapabilityContext s initialClaims = do
    st <- liftIO $ TokenStore <$> newIORef initialClaims
    pure $ CapabilityCtx s st


-- initializeCapabilityDB :: MonadIO m => FilePath -> Secret -> m CapabilityCtx
-- initializeCapabilityDB path secret = do
--     ctx <- newCapabilityContext secret
--     runReaderT (saveDB path) ctx
--     pure ctx


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


createClaims :: TokenM m r => Issuer -> (Maybe TTL) -> ResourceName -> Username -> Permissions -> m JWTClaimsSet
createClaims (Issuer issuer) ttl (ResourceName resourceName) (Username name) (Permissions perms) =
    let ttl_ = (\(TTL val) -> val) <$> ttl
    in do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
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


revokeClaims :: TokenM m r => JWTClaimsSet -> m ()
revokeClaims tok = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ modifyIORef store (L.delete tok)
    pure ()


revokeByUUID :: TokenM m r => Text -> m ()
revokeByUUID uuid = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ modifyIORef store (filter (\c -> getUUID c /= Just uuid))
    where
    getUUID = fmap stringOrURIToText . jti
    

replaceClaims :: TokenM m r => [JWTClaimsSet] -> m ()
replaceClaims newClaims = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ writeIORef store newClaims


listClaims :: TokenM m r => m [JWTClaimsSet]
listClaims = do
    (CapabilityCtx _ (TokenStore store)) <- hasCapabilityCtx <$> ask
    liftIO $ readIORef store


-- saveDB :: TokenM m r => FilePath -> m ()
-- saveDB path = do
--     (CapabilityCtx s _) <- hasCapabilityCtx <$> ask
--     claims <- listClaims
--     liftIO $ writeFile path $ toStrict $ encode $ CapDb claims
-- 
-- 
-- reloadDB :: TokenM m r => FilePath -> m ()
-- reloadDB path = do
--     (CapabilityCtx s (TokenStore store)) <- hasCapabilityCtx <$> ask
--     db <- (eitherDecode . fromStrict) <$> (liftIO $ readFile path)
--     case db of
--         Left err -> undefined
--         Right (CapDb claims) -> liftIO $ writeIORef store claims


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

