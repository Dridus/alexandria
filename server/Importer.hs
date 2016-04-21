module Importer where

import Application (makeFoundation)
import ClassyPrelude
import qualified Data.Aeson as Aeson
import Database.Persist (entityKey, entityVal, repsert)
import Database.Persist.Sql (runSqlPool)
import Foundation (appConnPool)
import Model
import Options.Applicative (Parser, strOption, short, long, metavar, strArgument, info, progDesc, fullDesc, execParser, helper)
import qualified Slack
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

data Opts = Opts
  { optSettings :: FilePath
  , optInputDir :: FilePath }

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption (short 's' ++ long "settings" ++ metavar "SETTINGS FILE")
  <*> strArgument (metavar "INPUT DIR")

importerMain :: IO ()
importerMain = do
  Opts {..} <- execParser $ info (helper <*> optsParser) (fullDesc ++ progDesc "Import a Slack archive from a directory")
  settings <- loadYamlSettings [unpack optSettings] [] useEnv
  foundation <- makeFoundation settings

  let connPool = appConnPool foundation
      withDB action = runSqlPool action connPool

  putStrLn "reading users.json"
  users    <- either (fail . ("while reading users.json: " <>)) pure . Aeson.eitherDecodeStrict'
          =<< readFile (optInputDir </> "users.json")

  let _ = users :: [Slack.User]

  putStrLn "reading channels.json"
  channels <- either (fail . ("while reading channels.json: " <>)) pure . Aeson.eitherDecodeStrict'
          =<< readFile (optInputDir </> "channels.json")

  let _ = channels :: [Slack.Channel]

  putStrLn "storing users"
  withDB $ mapM_ (uncurry repsert . (entityKey &&& entityVal) . fromSlackUser) users

  putStrLn "storing channels"
  withDB $ mapM_ (uncurry repsert . (entityKey &&& entityVal) . fromSlackChannel) channels

