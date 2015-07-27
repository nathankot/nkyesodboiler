module Import.Aws
       ( module Import
       , configurationFromSettings
       , managerSettingsFromSettings
       , s3SettingsFromSettings
       , s3ObjectPathFromSettings
       ) where

import           Aws                       as Import
import           Aws.Core                  as Import (Protocol (..),
                                                      defaultPort)
import qualified Aws.S3                    as S3
import           ClassyPrelude.Yesod
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as B
import qualified Data.Text                 as T (pack)
import qualified Data.Text.Encoding        as T (encodeUtf8)
import           Network.Connection        (TLSSettings (..))
import           Network.HTTP.Conduit      (mkManagerSettings)
import           Settings

configurationFromSettings :: MonadIO io
                             => AppSettings
                             -> io (Maybe Configuration)
configurationFromSettings s = liftIO $ runMaybeT $ do
  accessKeyId <- MaybeT $ return $ T.encodeUtf8 . T.pack <$> awsKey s
  secretAccessKey <- MaybeT $ return $ T.encodeUtf8 . T.pack <$> awsSecret s
  cr <- makeCredentials accessKeyId  secretAccessKey
  return Configuration { timeInfo = Timestamp
                       , credentials = cr
                       , logger = defaultLog Warning }

managerSettingsFromSettings :: AppSettings -> ManagerSettings
managerSettingsFromSettings s = (mkManagerSettings tlsSettings Nothing) {
    managerResponseTimeout = Just 120000000 } -- microseconds
  where
    tlsSettings  = TLSSettingsSimple
                   { settingDisableCertificateValidation = awsVerifyCertificate s == False
                   , settingDisableSession               = False
                   , settingUseServerName                = True }

s3SettingsFromSettings :: AppSettings -> S3.S3Configuration NormalQuery
s3SettingsFromSettings s = (S3.s3 HTTPS (B.pack $ awsEndpoint s) False)
                           { S3.s3Port = fromMaybe (defaultPort HTTPS) $ awsPort s }

-- | Get path to a given object name using the current bucket and endpoint
--   specified in app settings. This DOES NOT include a protocol.
s3ObjectPathFromSettings :: AppSettings -> String -> Maybe String
s3ObjectPathFromSettings s name = do
    bucket <- awsBucket s
    return $ bucket ++ "." ++ endpoint ++ "/" ++ name
  where
    endpoint = awsEndpoint s
