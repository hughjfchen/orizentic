{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Text
import           Data.Time
import           Options.Applicative
import           Web.JWT

import           LuminescentDreams.Capabilities


data Config = Config FilePath Command deriving Show
data Command = ListTokens
             | CreateToken Issuer (Maybe TTL) ResourceName Username Permissions
             | RevokeToken Text
             | InitializeDB
             deriving Show


cliParser :: Parser Config
cliParser = Config <$> strOption (long "db" <> help "Path to the capabilities database")
                   <*> subparser (
                            command "list" (info (pure ListTokens) (progDesc "list tokens"))
                        <>  command "create" (info parseCreateToken (progDesc "create a token"))
                        <>  command "revoke" (info parseRevokeToken (progDesc "revoke a token"))
                        <>  command "initialize" (info (pure InitializeDB) (progDesc "initialize a database")))

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

-- parseInitializeDB :: Parser Command
-- parseInitializeDB =
--     InitializeDB <$> (secret . pack <$> strOption (long "secret" <> help "The shared secret for this db"))


newtype CapM a = CapM (ReaderT CapabilityCtx IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CapabilityCtx)

runCapM :: CapabilityCtx -> CapM a -> IO a
runCapM ctx (CapM act) = runReaderT act ctx

main :: IO ()
main = do
    (Config path cmd) <- execParser (info (helper <*> cliParser)
                                    (fullDesc <> progDesc "description of the program"))
    
    case cmd of
        ListTokens -> do
            ctx <- newCapabilityContext (secret "")
            runCapM ctx $ do
                reloadDB path
                tokens <- listClaims
                forM_ tokens (liftIO . print)
        CreateToken issuer ttl resourceName userName perms -> do
            ctx <- newCapabilityContext (secret "")
            runCapM ctx $ do
                reloadDB path
                createClaims issuer ttl resourceName userName perms
                saveDB path
        RevokeToken uuid -> do
            ctx <- newCapabilityContext (secret "")
            runCapM ctx $ do
                reloadDB path
                revokeByUUID uuid
                saveDB path
        InitializeDB -> void $ initializeCapabilityDB path (secret "")


