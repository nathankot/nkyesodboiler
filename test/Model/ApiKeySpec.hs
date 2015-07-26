module Model.ApiKeySpec
  (spec) where

import TestImport
import Model.ApiKey

spec :: Spec
spec = withApp $ do

    describe "generateKey" $ do

        it "generates new keys each time" $ do
            k <- liftIO generateKey
            k2 <- liftIO generateKey
            assertEqual "keys are different" True (k /= k2)

        it "creates sufficiently long keys" $ do
            k <- liftIO generateKey
            assertEqual "key is long enough" True (length k >= 32)

    describe "generateApiKeyForUser" $ do

        it "creates an ApiKey record" $ do
            u <- runDB $ retrieve $ factoryUser id
            a <- runDB $ generateApiKeyForUser u
            assertEqual "api key is created" (entityKey u) (apiKeyUserId . entityVal $ a)

        it "ensures that there are no api key conflicts" $ do
            u <- runDB $ retrieve $ factoryUser id
            a <- runDB $ generateApiKeyForUser u
            let k = apiKeyValue $ entityVal a
            a2 <- runDB $ createApiKeyForUser u k
            assertEqual "second key is different" False (apiKeyValue (entityVal a2) == k)
