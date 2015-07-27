{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Foundation where

import           Control.Monad.Trans.Maybe
import           Database.Persist.Sql      (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Yesod.Core.Types          (Logger)
import qualified Yesod.Core.Unsafe         as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

--------------------------------------------------------------------------------
-- Internationalization
--------------------------------------------------------------------------------

mkMessage "App" "messages" "en"

--------------------------------------------------------------------------------
-- Routing & HTTP
--------------------------------------------------------------------------------

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level
      -- Don't ever log when testing
      | (appIsTesting $ appSettings app) == True = level == LevelWarn
                                                   || level == LevelError
      | otherwise = appShouldLogAll (appSettings app)
                    || level == LevelInfo
                    || level == LevelWarn
                    || level == LevelError

    makeLogger = return . appLogger

--------------------------------------------------------------------------------
-- Override unused functionality
--------------------------------------------------------------------------------

    -- Yesod's default sessions use an archaic cookie-based thing.
    -- We're going to do away with yesod sessions all together and opt
    -- for an api-key based method by overriding functions in @YesodAuth@
    makeSessionBackend _ = return Nothing
    defaultLayout _ = notFound
    -- The page to be redirected to when authentication is required.
    -- It is set to @Nothing@ so @403@ is responded when unauthorized.
    authRoute _ = Nothing
    -- Make it impossible to add static files, since we're not using them
    addStaticContent _ _ _ = return Nothing

--------------------------------------------------------------------------------
-- Request limitations
--------------------------------------------------------------------------------

    maximumContentLength _ _ = Just $ 2 * 1024 * 1024 -- 2mb

--------------------------------------------------------------------------------
-- Authentication
--------------------------------------------------------------------------------

    -- Routes not requiring authentication
    isAuthorized SessionsR _ = return Authorized
    isAuthorized UsersR _ = return Authorized
    -- Deny by default
    isAuthorized _ _ = return AuthenticationRequired

newtype CachedAuthenticatedUserId = CachedAuthenticatedUserId
    { unCachedAuthenticatedUserId :: Maybe UserId }
    deriving Typeable

-- | Retrieve a cached version of our authenticated user's id.
cachedMaybeAuthenticatedUserId :: Handler (Maybe UserId)
cachedMaybeAuthenticatedUserId = fmap unCachedAuthenticatedUserId $
                                 cached $
                                 fmap CachedAuthenticatedUserId
                                 maybeAuthenticatedUserId
  where
    maybeAuthenticatedUserId = runMaybeT $ do
        s <- MaybeT $ lookupHeader hAuthorization
        token <- MaybeT $ return $ stripPrefix "Bearer " s
        Entity _ a <- MaybeT $ runDB $ getBy $ UniqueApiKey (decodeUtf8 token)
        return $ apiKeyUserId a

--------------------------------------------------------------------------------
-- Datastore
--------------------------------------------------------------------------------

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
