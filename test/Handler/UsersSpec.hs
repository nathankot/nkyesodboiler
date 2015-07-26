module Handler.UsersSpec
    (spec) where

import TestImport
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H (member)

spec :: Spec
spec = withApp $ do

    describe "postUsersR" $ do

        it "creates a user" $ do
            makeRequest
            statusIs 201
            -- @retrieve@ implicitly tests that the user exists
            Entity _ u <- runDB $ retrieve $ selectFirst [UserEmail ==. email] []
            assertEqual "Verified is true" True $ userVerified u
                             
        it "sends back an api key" $ do
            makeRequest
            statusIs 201
            valueSatisfies "Response has api key value" $ \(Object v) ->
                let Object a = v ! "apiKey"
                    String s = a ! "value"
                in length s > 0
                             
        it "sends back the user" $ do
            makeRequest
            statusIs 201
            valueSatisfies "Response has users email" $ \(Object v) ->
                let Object u = v ! "user"
                    String e = u ! "email"
                in e == email

        it "doesnt send back the verification key" $ do
            makeRequest
            statusIs 201
            valueSatisfies "response doesn't have verKey" $ \(Object v) ->
                let Object u = v ! "user"
                in H.member "verKey" u == False

        it "doesnt send back the salted pass" $ do
            makeRequest
            statusIs 201
            valueSatisfies "response doesn't have pass" $ \(Object v) ->
                let Object u = v ! "user"
                in H.member "password" u == False

        it "responds with error when no email is provided" $ do
            makeRequestWithBody $ object [ "password" .= ("newpassword123"::Text) ]
            statusIs 400

        it "responds with error when no password is provided" $ do
            makeRequestWithBody $ object [ "email" .= (email::Text) ]
            statusIs 400

        it "responds with error on a short password" $ do
            makeRequestWithBody $ object [ "email" .= (email::Text)
                                         , "password" .= ("sho"::Text) ]
            statusIs 400

        it "responds with error on a invalid email" $ do
            makeRequestWithBody $ object [ "email" .= ("notanemail"::Text)
                                         , "password" .= ("newpassword123"::Text) ]
            statusIs 400

        it "responds with error on duplicate email" $ do
            Entity _ u <- runDB $ retrieve $ factoryUser id
            makeRequestWithBody $ object [ "email" .= (userEmail u :: Text)
                                         , "password" .= ("newpassword123"::Text) ]
            statusIs 400
            

    where
      email = "test@testing.com"
      makeRequestWithBody body = requestJSON $ setMethod "POST" >>
                                               setUrl UsersR >>
                                              (setRequestBody $ encode $ body)
      makeRequest = makeRequestWithBody $ object
                    [ "email" .= (email::Text)
                    , "password" .= ("newpassword123"::Text) ]
