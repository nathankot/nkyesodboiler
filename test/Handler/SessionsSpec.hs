module Handler.SessionsSpec
    (spec) where

import           Data.HashMap.Strict       ((!))
import qualified Data.Text                 as TS
import           Model.ApiKey
import           Network.HTTP.Types.Header
import           TestImport
import           Yesod.Auth.Email          (saltPass)

spec :: Spec
spec = withApp $ do

    describe "POST session" $ do

        let pass = "testing123456"

        it "responds with an api key" $ do
            salted <- liftIO $ saltPass pass
            Entity uid u <-
                runDB $ retrieve $ factoryUser $ \u ->
                u { userPassword = Just salted }

            requestJSON $ do
                setMethod "POST"
                setUrl SessionsR
                setRequestBody $ encode $ object
                               [ "email" .= userEmail u
                               , "password" .= ( pass::Text ) ]

            Entity _ a <- runDB $ retrieve $ selectFirst [ApiKeyUserId ==. uid] []

            valueSatisfies "Api key is returned" $ \(Object v) ->
                let Object apiKey = v ! "apiKey"
                in apiKey ! "value" == (String $ apiKeyValue a)

            valueSatisfies "User is returned" $ \(Object v) ->
                let Object user = v ! "user"
                in user ! "email" == (String $ userEmail u)


    describe "DELETE session" $ do

        it "removes the api key" $ do
            user@(Entity uid _) <- runDB $ retrieve $ factoryUser id
            Entity _ k <- runDB $ generateApiKeyForUser user

            requestJSON $ do
                setUrl SessionsR
                setMethod "DELETE"
                addRequestHeader (hAuthorization, encodeUtf8 $ "Bearer " `TS.append` apiKeyValue k)

            ma <- runDB $ selectFirst [ApiKeyUserId ==. uid] []
            assertEqual "API key no longer exists" False $ isJust ma

