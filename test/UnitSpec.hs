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
newContext = undefined

newtype CapSpecM a = CapSpecM (ReaderT Context IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

runCapSpec :: Context -> CapSpecM a -> IO a
runCapSpec ctx (CapSpecM act) = runReaderT act ctx

spec :: Spec
spec = describe "Capability Unit Tests" $ do
    it "can create a new token" $ do
        ctx <- newContext (secret "ctx")
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
        ctx <- newContext (secret "ctx")
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


    it "rejects tokens with an invalid secret" $ do
        ctx1 <- newContext (secret "ctx1")
        ctx2 <- newContext (secret "ctx2")
        token <- runCapSpec ctx1 $
            createToken (Issuer "test")
                        (TTL 3600)
                        (ResourceName "resource-1")
                        (Username "Savanni")
                        (Permissions ["read", "write", "grant"])
        let Just unverifiedJWT = decode $ encodeSigned HS256 (secret "ctx") (claims token)
        res <- runCapSpec ctx2 $ validateToken unverifiedJWT
        res `shouldBe` Nothing

    it "rejects tokens that are absent from the database" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity) <- runCapSpec ctx $ do
            tok <- createToken (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            let Just unverifiedJWT = decode $ encodeSigned HS256 (secret "ctx") (claims tok)
            revokeToken tok
            validity <- validateToken unverifiedJWT
            pure (tok, validity)
        Just tok `shouldNotBe` validity

    it "validates present tokens with a valid secret" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity) <- runCapSpec ctx $ do
            tok <- createToken (Issuer "test")
                               (TTL 3600)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            let Just unverifiedJWT = decode $ encodeSigned HS256 (secret "ctx") (claims tok)
            validity <- validateToken unverifiedJWT
            pure (tok, validity)
        Just tok `shouldBe` validity

    it "accepts authorization functions" $ pending

    it "rejects expired tokens" $ do
        ctx <- newContext (secret "ctx")
        (tok, validity1, validity2) <- runCapSpec ctx $ do
            tok <- createToken (Issuer "test")
                               (TTL 1)
                               (ResourceName "resource-1")
                               (Username "Savanni")
                               (Permissions ["read", "write", "grant"])
            let Just unverifiedJWT = decode $ encodeSigned HS256 (secret "ctx") (claims tok)
            validity1 <- validateToken unverifiedJWT
            liftIO $ threadDelay 2000000
            validity2 <- validateToken unverifiedJWT
            pure (tok, validity1, validity2)
        Just tok `shouldBe` validity1
        validity2 `shouldBe` Nothing

