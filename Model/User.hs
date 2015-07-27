module Model.User
( createUser
, checkPasswordSecurity
) where

import qualified Data.HashMap.Strict as H
import qualified Data.Text           as TS
import           Import

instance ToJSON (ResponseView (Entity User)) where
    -- Uses a whitelist approach.
    toJSON (ResponseView (Entity uid u)) =
        Object $
        H.insert "id" (toJSON uid) $
        H.delete "verKey" $
        H.delete "password" o
      where (Object o) = toJSON u

-- | Create a user from an @Entity@.
createUser :: ( MonadIO m
              , PersistUnique backend
              , PersistEntityBackend User ~ backend
              , Functor m)
           => User
           -> ReaderT backend m (Maybe (Entity User))

createUser user = do
    eu <- insertBy user
    mu <- case eu of Left e -> return $ Just e
                     Right uid -> fmap (Entity uid) <$> get uid
    return mu


checkPasswordSecurity :: Text -> IO (Either Text ())
checkPasswordSecurity x | TS.length x >= 6 = return $ Right ()
                        | otherwise = return $ Left "Password must be at least six characters"
