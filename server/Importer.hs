module Importer where

import Foundation (Foundation)
import Options.Applicative

data Opts = Opts
  { optSettings :: Text
  , optInputDir :: Text }

optsParser :: Parser Opts
optsParser = Opts
  <$> option auto (short 's' <> long "settings" <> metavar "SETTINGS FILE")
  <$> argument auto (metavar "INPUT DIR")

importerMain :: IO ()
importerMain = do
  Opts {..} <- execParser $ info (helper <*> optsParser) (fullDesc <> progDesc "Import a Slack archive from a directory")
  settings <- loadAppSettings optSettings
