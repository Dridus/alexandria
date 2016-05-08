{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger (liftLoc, runLoggingT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Writer.Class (tell)
import Data.Function (fix)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import Database.Persist.Sql (Migration, Single(Single, unSingle), connEscapeName, rawSql)
import Import hiding (Proxy)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
       ( Settings, defaultSettings, defaultShouldDisplayException, runSettings
       , setHost, setOnException, setPort, getPort )
import Network.Wai.Middleware.RequestLogger
       ( Destination(Logger), IPAddrSource(..), OutputFormat(..), destination, mkRequestLogger, outputFormat)
import Slack (MessageSubtype)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import TextShow (TextShow, showt)

import Handler.Common
import Handler.Home

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
    (appStaticDir appSettings)

  let mkFoundation appConnPool = App {..}
      tempFoundation           = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc                  = messageLoggerSource tempFoundation appLogger

  pool <- flip runLoggingT logFunc $ createPostgresqlPool
    (pgConnStr  $ appDatabaseConf appSettings)
    (pgPoolSize $ appDatabaseConf appSettings)

  flip runLoggingT logFunc $ do
    $logInfo $ "Checking connection to Postgres..."
    fix $ \ tryAgain ->
      catch ( do [Single version] <- runSqlPool (rawSql "SELECT version()" []) pool
                 $logInfo $ "Connected to Postgres: " <> version )
            ( \ (ex :: SomeException) ->
              do $logDebug $ "Failed to connect to Postgres: " <> tshow ex
                 threadDelay 1000000
                 tryAgain )

  runLoggingT (runSqlPool (runMigration $ createTypes >> migrateAll) pool) logFunc

  return $ mkFoundation pool

createTypes :: Migration
createTypes = do
  createEnum "message_subtype" (Proxy :: Proxy MessageSubtype)

tellSafeMigrationSql :: [Text] -> Migration
tellSafeMigrationSql = lift . tell . map (False,)

tellMigrationError :: [Text] -> Migration
tellMigrationError = tell

createEnum :: forall proxy a. (Bounded a, Enum a, Ord a, PersistField a, TextShow a) => Text -> proxy a -> Migration
createEnum typname _ = do
  escape <- asks connEscapeName
  let typnameDb = DBName typname
      -- FIXME?
      quoteValue = T.replace "'" "''" 
                 . (\ case PersistText t -> t
                           _ -> error "expected enums to make strings when converted")
                 . toPersistValue
      allValues = setFromList [minBound .. maxBound] :: Set a
  (lift . lift $ rawSql "SELECT oid, typtype FROM pg_type WHERE typname = ?" [toPersistValue typname]) >>= \ case
    [] -> 
      let quotedValues = intercalate ", " (map (\ value -> "'" <> quoteValue value <> "'") . toList $ allValues)
      in tellSafeMigrationSql ["CREATe TYPE " <> escape typnameDb <> " AS ENUM(" <> quotedValues <> ")"]

    [(Single oid, Single "e")] -> do
      existingValues <- setFromList . map unSingle
                    <$> (lift . lift $ rawSql "SELECT enumlabel FROM pg_enum WHERE enumtypid = ?" [oid])
      let missingValues = allValues `difference` existingValues
          extraValues = existingValues `difference` allValues

          addValue value = "ALTER TYPE " <> escape typnameDb <> " ADD VALUE '" <> quoteValue value <> "'"

      unless (null extraValues) $
        tellMigrationError [typname <> " has extra enum values " <> showt (toList extraValues)]
      unless (null missingValues) $ do
        tellSafeMigrationSql
          $  ["COMMIT"] -- FIXME? dunno why alter tables go fine but not alter type, but this seems to fix it up
          <> map addValue (toList missingValues)
          <> ["BEGIN TRANSACTION"] -- FIXME as above

      pure ()

    [(Single _, Single other)] ->
      tellMigrationError $ [typname <> " exists but is of type " <> other <> " not e (enum)"]

    rows ->
      tellMigrationError $ ["Got multiple results for the " <> typname <> " query?"] ++ map tshow rows

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger def
    { outputFormat =
      if appDetailedRequestLogging $ appSettings foundation
        then Detailed True
        else Apache
          (if appIpFromHeader $ appSettings foundation
            then FromFallback
            else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation)
  $ setHost (appHost $ appSettings foundation)
  $ setOnException (\_req e ->
      when (defaultShouldDisplayException e) $ messageLoggerSource
          foundation
          (appLogger foundation)
          $(qLocation >>= liftLoc)
          "yesod"
          LevelError
          (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <- loadYamlSettingsArgs
    -- fall back to compile-time values, set to [] to require values at runtime
    [configSettingsYmlValue]

    -- allow environment variables to override
    useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  -- Run the application with Warp
  runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
