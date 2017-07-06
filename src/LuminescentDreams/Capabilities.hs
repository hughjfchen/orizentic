{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module LuminescentDreams.Capabilities where

import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader(..))
import           Data.IORef
import           Data.Text
import           Data.Time
import           Web.JWT

{- What is a token store? Is it anything more than a list of currently valid tokens? -}

newtype ResourceName = ResourceName Text
newtype Permissions = Permissions [Text]
newtype Issuer = Issuer Text
newtype TTL = TTL DiffTime
newtype Username = Username Text

instance Eq (JWT VerifiedJWT) where
    j1 == j2 = claims j1 == claims j2

newtype TokenStore = TokenStore [IORef (JWT VerifiedJWT)]

data CapabilityCtx = CapabilityCtx Secret TokenStore

class HasCapabilityCtx ctx where
    hasCapabilityCtx :: ctx -> CapabilityCtx

type TokenM m r = (MonadIO m, MonadReader r m, HasCapabilityCtx r)

validateToken :: TokenM m r => JWT UnverifiedJWT -> m (Maybe (JWT VerifiedJWT))
validateToken = undefined

checkAuthorizations :: (ResourceName -> Permissions -> Bool) -> JWT VerifiedJWT -> Bool
checkAuthorizations = undefined

createToken :: TokenM m r => Issuer -> TTL -> ResourceName -> Username -> Permissions -> m (JWT VerifiedJWT)
createToken = undefined

revokeToken :: TokenM m r => JWT VerifiedJWT -> m ()
revokeToken = undefined

listTokens :: TokenM m r => m [JWT VerifiedJWT]
listTokens = undefined

