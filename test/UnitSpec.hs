{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitSpec where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader   (MonadReader(..), ReaderT(..))
import           Test.Hspec

import           LuminescentDreams.Capabilities

newtype Context = Context TokenStore

newContext :: IO Context
newContext = undefined


instance HasTokenStore Context where
    hasTokenStore (Context ts) = ts

newtype CapSpecM a = CapSpecM (ReaderT Context IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

runCapSpec :: Context -> CapSpecM a -> IO a
runCapSpec ctx (CapSpecM act) = runReaderT act ctx

spec :: Spec
spec = describe "Capability Unit Tests" $ do
    it "can create a new token" $ do
        ctx <- newContext
        (tok, tok2, tokList) <- runCapSpec ctx $ do
            tok <- createToken (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            tok2 <- createToken (Issuer "test")
                                (TTL 3600)
                                (ResourceName "resource-2")
                                (Username "Savanni")
                                (Permissions ["read", "write", "grant"])
            tokList <- listTokens
            pure (tok, tok2, tokList)
        tokList `shouldSatisfy` elem tok
        tokList `shouldSatisfy` elem tok2
        length tokList `shouldBe` 2


    it "can revoke a token" $ do
        ctx <- newContext
        (tok, tok2, tokList) <- runCapSpec ctx $ do
            tok <- createToken (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            tok2 <- createToken (Issuer "test")
                                (TTL 3600)
                                (ResourceName "resource-2")
                                (Username "Savanni")
                                (Permissions ["read", "write", "grant"])
            revokeToken tok
            tokList <- listTokens
            pure (tok, tok2, tokList)

        tokList `shouldNotSatisfy` elem tok
        tokList `shouldSatisfy` elem tok2
        length tokList `shouldBe` 1


    it "rejects tokens with an invalid secret" $ pending

    it "rejects tokens that are absent from the database" $ pending

    it "validates present tokens with a valid secret" $ pending

    it "accepts authorization functions" $ pending

    it "rejects expired tokens" $ pending

