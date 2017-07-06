{-# LANGUAGE ConstraintKinds    #-}
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

newtype Token = Token (JWT VerifiedJWT) deriving (Show)

instance Eq Token where
    (Token j1) == (Token j2) = claims j1 == claims j2

newtype TokenStore = TokenStore [IORef Token]

class HasTokenStore ctx where
    hasTokenStore :: ctx -> TokenStore

type TokenM m r = (MonadIO m, MonadReader r m, HasTokenStore r)

validateToken :: TokenM m r => Secret -> JWT UnverifiedJWT -> m (Maybe Token)
validateToken = undefined

checkAuthorizations :: (ResourceName -> Permissions -> Bool) -> Token -> Bool
checkAuthorizations = undefined

createToken :: TokenM m r => Issuer -> TTL -> ResourceName -> Username -> Permissions -> m Token
createToken = undefined

revokeToken :: TokenM m r => Token -> m ()
revokeToken = undefined

listTokens :: TokenM m r => m [Token]
listTokens = undefined

