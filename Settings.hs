{-# LANGUAGE StandaloneDeriving, RankNTypes #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?), withScientific)
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Network (PortNumber)
import Network.Wai.Handler.Warp    (HostPreference)

import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appIsTesting              :: Bool
    -- ^ Set to true when running tests

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    , appSmtpHost               :: Maybe String
    , appSmtpUser               :: Maybe String
    , appSmtpPassword           :: Maybe String
    , appSmtpPort               :: PortNumber

    , awsKey                     :: Maybe String
    , awsSecret                  :: Maybe String
    , awsBucket                  :: Maybe String
    , awsEndpoint                :: String
    , awsPort                    :: Maybe Int
    , awsLocationConstraint      :: String
    , awsVerifyCertificate       :: Bool

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "app-root"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appIsTesting              <- o .:? "app-is-testing"   .!= False
        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appSmtpHost               <- o .:? "smtp-host"
        appSmtpUser               <- o .:? "smtp-user"
        appSmtpPassword           <- o .:? "smtp-password"
        appSmtpPort               <- withScientific "PortNumber" (pure . floor) =<< (o .: "smtp-port")

        awsKey                    <- o .:? "aws-key"
        awsSecret                 <- o .:? "aws-secret"
        awsBucket                 <- o .:? "aws-bucket"
        awsPort                   <- o .:? "aws-port"
        awsEndpoint               <- o .:  "aws-endpoint"
        awsLocationConstraint     <- o .:  "aws-location-constraint"
        awsVerifyCertificate      <- o .:  "aws-verify-certificate"

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        return AppSettings {..}

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
