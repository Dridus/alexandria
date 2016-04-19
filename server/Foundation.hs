module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appConnPool    :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend
      120    -- timeout in minutes
      "config/client_session_key.aes"

  yesodMiddleware = defaultYesodMiddleware

  isAuthorized FaviconR _ = pure Authorized
  isAuthorized RobotsR _ = pure Authorized
  isAuthorized _ _ = pure Authorized

  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = pure . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
