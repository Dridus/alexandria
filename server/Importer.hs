module Importer where

import Application (makeFoundation)
import ClassyPrelude
import Control.Lens (view)
import qualified Data.Aeson as Aeson
import Database.Persist (Entity(Entity), Filter, entityKey, entityVal, getBy, insert, replace, repsert)
import Database.Persist.Sql (deleteWhereCount, runSqlPool)
import Foundation (appConnPool)
import Model
import Options.Applicative (Parser, strOption, short, long, metavar, strArgument, info, progDesc, fullDesc, execParser, helper, switch, help)
import qualified Slack
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath.Posix (takeExtensions)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

data Opts = Opts
  { optSettings :: FilePath
  , optInputDir :: FilePath
  , optFresh    :: Bool
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption (short 's' ++ long "settings" ++ metavar "SETTINGS FILE")
  <*> strArgument (metavar "INPUT DIR")
  <*> switch (long "fresh" ++ help "Clear out all existing channels, users, and messages before importing")

importerMain :: IO ()
importerMain = do
  Opts {..} <- execParser $ info (helper <*> optsParser) (fullDesc ++ progDesc "Import a Slack archive from a directory")
  settings <- loadYamlSettings [unpack optSettings] [] useEnv
  foundation <- makeFoundation settings

  let connPool = appConnPool foundation
      withDB action = runSqlPool action connPool
      repsertEntity = uncurry repsert . (entityKey &&& entityVal)

  when optFresh . withDB $ do
    putStr "deleting reactions..."
    reactionsDeleted <- deleteWhereCount ([] :: [Filter MessageReaction])
    putStrLn $ " " <> tshow reactionsDeleted <> " deleted."

    putStr "deleting attachments..."
    attachmentsDeleted <- deleteWhereCount ([] :: [Filter MessageAttachment])
    putStrLn $ " " <> tshow attachmentsDeleted <> " deleted."

    putStr "deleting messages..."
    messagesDeleted <- deleteWhereCount ([] :: [Filter Message])
    putStrLn $ " " <> tshow messagesDeleted <> " deleted."

    putStr "deleting users..."
    usersDeleted <- deleteWhereCount ([] :: [Filter User])
    putStrLn $ " " <> tshow usersDeleted <> " deleted."

    putStr "deleting channels..."
    channelsDeleted <- deleteWhereCount ([] :: [Filter Channel])
    putStrLn $ " " <> tshow channelsDeleted <> " deleted."

  putStrLn "reading users.json"
  users    <- either (fail . ("while reading users.json: " <>)) pure . Aeson.eitherDecodeStrict'
          =<< readFile (optInputDir </> "users.json")

  let _ = users :: [Slack.User]

  putStrLn "reading channels.json"
  channels <- either (fail . ("while reading channels.json: " <>)) pure . Aeson.eitherDecodeStrict'
          =<< readFile (optInputDir </> "channels.json")

  let _ = channels :: [Slack.Channel]

  putStrLn "storing users"
  withDB $ mapM_ (repsertEntity . fromSlackUser) users

  putStrLn "storing channels"
  withDB $ mapM_ (repsertEntity . fromSlackChannel) channels

  forM_ channels $ \ channel -> do
    putStrLn $ "Starting on channel " <> view Slack.channelName channel
    let channelDir = optInputDir </> unpack (view Slack.channelName channel)
    files <- filter ((== ".json") . takeExtensions)
         <$> ( filterM doesFileExist . map (channelDir </>)
           =<< getDirectoryContents channelDir )

    forM_ files $ \ file -> do
      putStr $ " ... " <> pack file
      -- FIXME? this doesn't even vaguely attempt to stream
      bytes <- readFile file
      messages <- either fail pure $ Aeson.eitherDecodeStrict' bytes
      let _ = messages :: [Slack.Message]
      putStrLn $ " (" <> tshow (length messages) <> " items)"

      withDB $ do
        forM_ messages $ \ slackMessage ->
          case Model.fromSlackMessage slackMessage of
            Nothing -> pure ()
            Just modelMessage -> do
              key <- getBy (UniqueMessage (view messageChannel modelMessage) (view messageTs modelMessage)) >>= \ case
                Just (Entity k _) -> replace k modelMessage *> pure k
                Nothing           -> insert modelMessage

              -- mapM_ insert $ Model.attachmentsFromSlackMessage =<< messages
              -- mapM_ insert $ Model.reactionsFromSlackMessage =<< messages
              --
              pure ()

