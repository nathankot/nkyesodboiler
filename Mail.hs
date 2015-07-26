module Mail
  ( module Network.Mail.SMTP
  , sendMailWithAppSettings ) where

import           Data.Maybe
import           Import.NoFoundation
import           Network.Mail.Mime
import           Network.Mail.SMTP

sendMailWithAppSettings :: AppSettings -> Mail -> IO ()
sendMailWithAppSettings s
  | appIsTesting s = const $ return () -- Don't do anything when testing
  | isJust musername && isJust mpassword && isJust mhost =
      sendMailWithLogin' (fromJust mhost) port (fromJust musername) (fromJust mpassword)
  | isJust mhost = sendMail' (fromJust mhost) port
  | otherwise = const $ return ()
  where mhost = appSmtpHost s
        port = appSmtpPort s
        musername = appSmtpUser s
        mpassword = appSmtpPassword s
