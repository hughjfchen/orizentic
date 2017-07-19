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
import           Options.Applicative
import           System.IO.Error            (isDoesNotExistError)
import           Web.JWT

import           LuminescentDreams.Orizentic


data Config = Config FilePath Command deriving Show
data Command = ListTokens
             | CreateToken Secret Issuer (Maybe TTL) ResourceName Username Permissions
             | RevokeToken Text
             | EncodeToken Secret Text
             deriving Show


cliParser :: Parser Config
cliParser = Config <$> strOption (long "db" <> help "Path to the capabilities database")
                   <*> subparser (
                            command "list" (info (pure ListTokens) (progDesc "list tokens"))
                        <>  command "create" (info parseCreateToken (progDesc "create a token"))
                        <>  command "revoke" (info parseRevokeToken (progDesc "revoke a token"))
                        <>  command "encode" (info parseEncodeToken (progDesc "encode a token with a secret")))

parseRevokeToken :: Parser Command
parseRevokeToken =
    RevokeToken <$> (pack <$> strOption (long "id" <> help "Token ID"))

parseCreateToken :: Parser Command
parseCreateToken =
    CreateToken <$> (secret . pack <$> strOption (long "secret" <> help "The secret to use for encoding the token"))
                <*> (Issuer . pack <$> strOption (long "issuer" <> help "Token issuer (typically the owner)"))
                <*> optional (TTL . fromRational <$> option auto (long "ttl" <> help "Seconds until the token expires"))
                <*> (ResourceName . pack <$> strOption (long "resource" <> help "Name of the resource"))
                <*> (Username . pack <$> strOption (long "name" <> help "Name of the user"))
                <*> (Permissions . splitOn "," . pack <$> strOption (long "perms" <> help "Permissions"))

parseEncodeToken :: Parser Command
parseEncodeToken =
    EncodeToken <$> (secret . pack <$> strOption (long "secret" <> help "The secret to use for encoding the token"))
                <*> (pack <$> strOption (long "id" <> help "Token ID"))

newtype CapM a = CapM (ReaderT OrizenticCtx IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader OrizenticCtx)

runCapM :: OrizenticCtx -> CapM a -> IO a
runCapM ctx (CapM act) = runReaderT act ctx

main :: IO ()
main = do
    (Config path cmd) <- execParser (info (helper <*> cliParser)
                                    (fullDesc <> progDesc "description of the program"))

    claimsLst <- loadDB path
    case claimsLst of
        Left err -> error $ show err
        Right claims_ -> do
            ctx <- newOrizenticCtx (secret "") claims_
            case cmd of
                ListTokens -> runCapM ctx $ listClaims >>= \t -> forM_ t (liftIO . print)
                CreateToken s issuer ttl resourceName userName perms -> runCapM ctx $ do
                    claim <- createClaims issuer ttl resourceName userName perms
                    listClaims >>= \c -> liftIO $ saveDB c path
                    liftIO $ print $ encodeSigned HS256 s claim
                RevokeToken uuid -> runCapM ctx $ do
                    revokeByUUID uuid
                    listClaims >>= \c -> liftIO $ saveDB c path
                EncodeToken s uuid -> runCapM ctx $ do
                    claim <- findClaims uuid
                    case claim of
                        Nothing -> error "claim not found"
                        Just claim_ -> liftIO $ print $ encodeSigned HS256 s claim_

loadDB :: FilePath -> IO (Either String [JWTClaimsSet])
loadDB path =
    catch ((eitherDecode . fromStrict) <$> readFile path)
          (\e -> if isDoesNotExistError e
                    then pure $ Right []
                    else throw e)

saveDB :: [JWTClaimsSet] -> FilePath -> IO ()
saveDB claimsLst path = writeFile path (toStrict $ encode claimsLst)

