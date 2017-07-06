{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitSpec where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader   (MonadReader(..), ReaderT(..))
import           Test.Hspec
import           Web.JWT

import           LuminescentDreams.Capabilities


newtype Context = Context CapabilityCtx

instance HasCapabilityCtx Context where
    hasCapabilityCtx (Context c) = c

newContext :: Secret -> IO Context
newContext secret = Context <$> newCapabilityContext secret

newtype CapSpecM a = CapSpecM (ReaderT Context IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

runCapSpec :: Context -> CapSpecM a -> IO a
runCapSpec ctx (CapSpecM act) = runReaderT act ctx

spec :: Spec
spec = describe "Capability Unit Tests" $ do
    it "can create a new token" $ do
        ctx <- newContext (secret "ctx")
        (tok, tok2, tokList) <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            tok2 <- createClaims (Issuer "test")
                                (TTL 3600)
                                (ResourceName "resource-2")
                                (Username "Savanni")
                                (Permissions ["read", "write", "grant"])
            tokList <- listClaims
            pure (tok, tok2, tokList)
        tokList `shouldSatisfy` elem tok
        tokList `shouldSatisfy` elem tok2
        length tokList `shouldBe` 2


    it "can revoke a token" $ do
        ctx <- newContext (secret "ctx")
        (tok, tok2, tokList) <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            tok2 <- createClaims (Issuer "test")
                                (TTL 3600)
                                (ResourceName "resource-2")
                                (Username "Savanni")
                                (Permissions ["read", "write", "grant"])
            revokeClaims tok
            tokList <- listClaims
            pure (tok, tok2, tokList)

        tokList `shouldNotSatisfy` elem tok
        tokList `shouldSatisfy` elem tok2
        length tokList `shouldBe` 1


    it "rejects tokens with an invalid secret" $ do
        ctx1 <- newContext (secret "ctx1")
        ctx2 <- newContext (secret "ctx2")
        Just unverifiedJWT <- runCapSpec ctx1 $ do
            token <- createClaims (Issuer "test")
                                 (TTL 3600)
                                 (ResourceName "resource-1")
                                 (Username "Savanni")
                                 (Permissions ["read", "write", "grant"])
            decode <$> encodeToken token
        res <- runCapSpec ctx2 $ validateToken unverifiedJWT
        res `shouldBe` Nothing

    it "rejects tokens that are absent from the database" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity) <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            revokeClaims tok
            validity <- validateToken unverifiedJWT
            pure (tok, validity)
        validity `shouldBe` Nothing

    it "validates present tokens with a valid secret" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity) <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            validity <- validateToken unverifiedJWT
            pure (tok, validity)
        (claims <$> validity) `shouldBe` Just tok

    it "rejects expired tokens" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity1, validity2) <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 1)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            validity1 <- validateToken unverifiedJWT
            liftIO $ threadDelay 2000000
            validity2 <- validateToken unverifiedJWT
            pure (tok, validity1, validity2)
        (claims <$> validity1) `shouldBe` Just tok
        validity2 `shouldBe` Nothing

    it "authorizes a token with the correct resource and permissions" $ do
        ctx <- newContext (secret "ctx")
        res <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            Just jwt <- validateToken unverifiedJWT
            pure $ checkAuthorizations (\rn perms -> (rn == ResourceName "resource-1") && (perms `hasPermission` "grant"))
                                       jwt
        res `shouldBe` True

    it "rejects a token with the incorrect permissions" $ do
        ctx <- newContext (secret "ctx")
        res <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            Just jwt <- validateToken unverifiedJWT
            pure $ checkAuthorizations (\rn perms -> (rn == ResourceName "resource-1") && (perms `hasPermission` "grant"))
                                       jwt
        res `shouldBe` False

    it "rejects a token with the incorrect resource name" $ do
        ctx <- newContext (secret "ctx")
        res <- runCapSpec ctx $ do
            tok <- createClaims (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource")
                               (Username "Savanni")
                               (Permissions ["read, write, grant"])
            Just unverifiedJWT <- decode <$> encodeToken tok
            Just jwt <- validateToken unverifiedJWT
            pure $ checkAuthorizations (\rn perms -> (rn == ResourceName "resource-1") && (perms `hasPermission` "grant"))
                                       jwt
        res `shouldBe` False


