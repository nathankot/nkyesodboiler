module Handler.Sessions where

import           Control.Monad.Trans.Maybe
import           Import
import           Model.ApiKey
import           Model.User
import           Yesod.Auth.Email          (isValidPass)

-- | Re-implementation of 'auth-email's login handler,
--   heavily based on: https://goo.gl/yEQyqt
--   except instead of delegating authentication back to @YesodAuth@,
--   it generates and responds with an api key.
postSessionsR :: Handler Value
postSessionsR = do
    SessionRequest email pass <-
        requireJsonBody :: Handler SessionRequest

    mu <- runMaybeT $ do
        Entity uid u <- MaybeT $ runDB $ getBy $ UniqueUser email
        realpass <- MaybeT $ return $ userPassword u
        let verstatus = userVerified u

        if isValidPass pass realpass && verstatus
           then (MaybeT . runDB . get) uid >>= MaybeT . return . Just . Entity uid
           else MaybeT $ return Nothing

    case mu of
        -- If we have a user, that mean this user has authenticated
        -- successfully
        Just user -> do
            a <- runDB $ generateApiKeyForUser user
            sendResponseStatus status201 $ object
              [ "apiKey" .= ResponseView a
              , "user" .= ResponseView user ]

        -- Otherwise, assume that authentication didn't go so well
        _ -> do
          mr <- getMessageRender
          sendResponseStatus status401 $
            object ["message" .= mr MsgSessionErrorInvalidLogin]

-- | Delete a session and remove it's API key.
--   This is effectively logging out.
deleteSessionsR :: Handler Value
deleteSessionsR = do
    _ <- runMaybeT $ do
        authorization <- MaybeT $ lookupHeader hAuthorization
        k <- MaybeT $ return $ stripPrefix "Bearer " authorization
        Entity aid _ <- MaybeT . runDB . getBy $ UniqueApiKey (decodeUtf8 k)
        MaybeT $ Just <$> (runDB $ delete aid)

    sendResponseStatus status204 $ Null

data SessionRequest = SessionRequest { _email    :: Text
                                     , _password :: Text }

instance FromJSON SessionRequest where
    parseJSON (Object v) = SessionRequest
                               <$> v .: "email"
                               <*> v .: "password"
    parseJSON _ = mzero

