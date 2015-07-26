module TestFactory where

import ClassyPrelude
import Database.Persist
import Model
import Model.Types
import Model.User (createUser)

factoryUser :: ( MonadIO m
               , PersistUnique backend
               , PersistEntityBackend User ~ backend
               , Functor m)
            => (User -> User) -- ^ Chance to manipulate the user
            -> ReaderT backend m (Maybe (Entity User))

factoryUser transform = createUser $ transform user
    where user = User { userEmail = "default@email.com"
                      , userPassword = Just "notapasswordhash"
                      , userVerKey = Nothing
                      , userVerified = True
                      , userCreated = Nothing }
