module Handler.Users where

import           Control.Monad.Trans.Either
import           Import
import           Model.ApiKey
import           Model.User
import           Text.Email.Validate        (canonicalizeEmail)
import           Yesod.Auth.Email           (saltPass)

-- | Data type to embody the request. Yesod will automatically
--   throw 400 if the request data does not match.
--   We don't serialize directly to a User in this case because
--   the password needs to be salted, and we want to give meaningful
--   error messages as responses. Also, it's dangerous to try and parse
--   request data into a user because there are things we don't want
--   the user to control.
data UserRequest = UserRequest { _email    :: Text
                               , _password :: Text }

instance FromJSON UserRequest where
    parseJSON (Object v) = UserRequest
                              <$> v .: "email"
                              <*> v .: "password"
    parseJSON _ = mzero

-- | Custom-rolled user registration that ensures a password is chosen
--   BEFORE the verification process.
--   Use this as a reference for development: https://goo.gl/ks7PwA
--   Why roll our own? Because yesod-auth is uncustomizable, and has 0
--   support for Bearer token authentication.
postUsersR :: Handler Value
postUsersR = do
    userreq <- requireJsonBody :: Handler UserRequest
    eu <- runEitherT $ newUser userreq
    case eu of
      Left msg -> sendResponseStatus status400 $ object ["message" .= msg]
      Right u -> do
          muid <- runDB $ insertUnique u
          case muid of
            Nothing ->
                sendResponseStatus status400 $
                    object ["message" .= ("Email already exists"::Text)]
            Just uid -> do
                apikey <- runDB $ generateApiKeyForUser $ Entity uid u
                sendResponseStatus status201 $ object
                  [ "apiKey" .= (ResponseView apikey)
                  , "user" .= (ResponseView $ Entity uid u) ]

-- | Validate and create a new user, wrapped in @EitherT@
newUser :: UserRequest -> EitherT Text Handler User
newUser userreq = do
    -- Check the password
    _ <- EitherT . liftIO . checkPasswordSecurity $ _password userreq
    salted <- EitherT . liftIO $ Right <$> saltPass (_password userreq)
    -- Check the email
    let encodedemail = encodeUtf8 $ _email userreq
    email' <- hoistEither $ maybeEither "Invalid email" $ canonicalizeEmail encodedemail
    now <- EitherT . liftIO $ Right <$> getCurrentTime
    let email = decodeUtf8 email'
    -- Make the user
    hoistEither . Right $ User { userEmail = email
                               , userPassword = Just salted
                               , userVerKey = Nothing
                                 -- For now, verify all new users.
                               , userVerified = True
                               , userCreated = Just now }
  where
    maybeEither msg = maybe (Left msg) Right
