-- | Some tools for handlers
module Handler.Extension

       ( requireJsonEntity
       , requireUpdatedJsonEntity
       , noWhitelist
       , whitelist
       , fromMaybeM
       , requireCachedAuthenticatedUserId
       ) where

import           Data.Aeson              as A
import           Data.Aeson.Parser       as AP
import           Data.Aeson.Types        (Pair)
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.Conduit.Lift
import           Data.HashMap.Strict     as H
import           Foundation
import           Import.NoFoundation
import           Model.Extension

-- | Syntatic sugar, alias for @Just@
whitelist :: [Text] -> Maybe [Text]
whitelist = Just

-- | Syntatic sugar, alias for @Nothing@
noWhitelist :: Maybe [Text]
noWhitelist = Nothing

-- | Merge in the request object with an existing entity,
--   taking care to only consider whitelisted properties.
requireUpdatedJsonEntity :: (MonadHandler m,
                             FromJSON e,
                             ToJSON e,
                             Updatable e)
                         => Entity e -- ^ The existing entity
                         -> m e

requireUpdatedJsonEntity (Entity _ o) =
    requireJsonEntity defs (Just wl)
  where
    Object defs' = toJSON o
    defs = H.toList defs'
    wl = updatableProps o

-- | Retrieve a entity from the request body.
--   Using the given Aeson pair's as defaults for
--   missing values (useful for defaulting a userId.)
requireJsonEntity :: (MonadHandler m, FromJSON e) =>
                     [Pair]       -> -- ^ Default pair of values used to
                                     --   construct the entity
                     Maybe [Text] -> -- ^ A whitelist of allowed keys.
                                     --   This DOES NOT affect the defaults
                                     --   and is a good way to sanitize user input
                     m e

requireJsonEntity defs wl = do
    ra <- parseJsonEntity
    case ra of
      Error s -> invalidArgs [pack s]
      Success a -> return a
  where
    filterf k _ = maybe True (k `elem`) wl
    dofilter =  H.filterWithKey filterf
    parseJsonEntity = do
        eValue <- rawRequestBody $$ runCatchC (sinkParser AP.value')
        return $ case eValue of
            Left e -> A.Error $ show e
            Right (Object o) -> fromJSON (Object $ dofilter o `H.union` H.fromList defs)
            -- Can't do a merge if the resulting body value isn't an object
            Right v -> fromJSON v

-- | Unwrap a maybe wrapped in a monad. Useful for getting
--   I/O results whilst fallbacking to a response/value
--   This is superior to `liftM2 fromMaybe` because it's
--   lazy.
fromMaybeM :: Monad m =>  m a -> m (Maybe a) -> m a
fromMaybeM e h = do
  h' <- h
  case h' of
    Just a -> return a
    Nothing -> e

requireCachedAuthenticatedUserId :: Handler UserId
requireCachedAuthenticatedUserId = fromMaybeM notAuthenticated cachedMaybeAuthenticatedUserId
