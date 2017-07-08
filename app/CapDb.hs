{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Prelude hiding (readFile, writeFile)

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson                 (eitherDecode, encode)
import           Data.ByteString            (readFile, writeFile)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Monoid
import           Data.Text
import           Data.Time
import           Options.Applicative
import           System.IO.Error            (isDoesNotExistError)
import           Web.JWT

import           LuminescentDreams.Capabilities


data Config = Config FilePath Command deriving Show
data Command = ListTokens
             | CreateToken Issuer (Maybe TTL) ResourceName Username Permissions
             | RevokeToken Text
             deriving Show


cliParser :: Parser Config
cliParser = Config <$> strOption (long "db" <> help "Path to the capabilities database")
                   <*> subparser (
                            command "list" (info (pure ListTokens) (progDesc "list tokens"))
                        <>  command "create" (info parseCreateToken (progDesc "create a token"))
                        <>  command "revoke" (info parseRevokeToken (progDesc "revoke a token")))

parseRevokeToken :: Parser Command
parseRevokeToken =
    RevokeToken <$> (pack <$> strOption (long "id" <> help "Token ID"))

parseCreateToken :: Parser Command
parseCreateToken =
    CreateToken <$> (Issuer . pack <$> strOption (long "issuer" <> help "Token issuer (typically the owner)"))
                <*> optional (TTL . fromRational <$> option auto (long "ttl" <> help "Seconds until the token expires"))
                <*> (ResourceName . pack <$> strOption (long "resource" <> help "Name of the resource"))
                <*> (Username . pack <$> strOption (long "name" <> help "Name of the user"))
                <*> (Permissions . splitOn "," . pack <$> strOption (long "perms" <> help "Permissions"))


newtype CapM a = CapM (ReaderT CapabilityCtx IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CapabilityCtx)

runCapM :: CapabilityCtx -> CapM a -> IO a
runCapM ctx (CapM act) = runReaderT act ctx

main :: IO ()
main = do
    (Config path cmd) <- execParser (info (helper <*> cliParser)
                                    (fullDesc <> progDesc "description of the program"))

    claimsLst <- loadDB path
    case claimsLst of
        Left err -> undefined
        Right claims_ -> do
            ctx <- newCapabilityContext (secret "") claims_
            case cmd of
                ListTokens -> do
                    runCapM ctx $ listClaims >>= \t -> forM_ t (liftIO . print)
                CreateToken issuer ttl resourceName userName perms -> do
                    runCapM ctx $ do
                        createClaims issuer ttl resourceName userName perms
                        listClaims >>= \c -> liftIO $ saveDB c path
                RevokeToken uuid -> do
                    runCapM ctx $ do
                        revokeByUUID uuid
                        listClaims >>= \c -> liftIO $ saveDB c path

loadDB :: FilePath -> IO (Either String [JWTClaimsSet])
loadDB path = do
    catch ((eitherDecode . fromStrict) <$> readFile path)
          (\e -> if isDoesNotExistError e
                    then pure $ Right []
                    else throw e)

saveDB :: [JWTClaimsSet] -> FilePath -> IO ()
saveDB claimsLst path = writeFile path (toStrict $ encode claimsLst)

