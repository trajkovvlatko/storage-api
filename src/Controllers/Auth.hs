module Controllers.Auth
  ( login
  , register
  ) where

import GHC.Generics (Generic)
import Web.Scotty (ActionM, text, liftAndCatchIO, param, json, status)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (Text)
import Models.User (createUser, User (uId))
import ClassyPrelude (unpack, pack, Utf8(encodeUtf8), IsString (fromString), Text)
import Data.Aeson (ToJSON(toJSON, toEncoding), KeyValue((.=)), pairs)

newtype ErrorResponse = ErrorResponse { eMessage :: String } deriving Generic

instance ToJSON ErrorResponse where
  toEncoding (ErrorResponse eMessage) =
    pairs $ "message" .= eMessage

newtype TokenResponse = TokenResponse { uToken :: ClassyPrelude.Text } deriving Generic

instance ToJSON TokenResponse where
  toEncoding (TokenResponse uToken) =
    pairs $ "token" .= uToken

register :: ActionM ()
register = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"

  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing   -> json $ ErrorResponse { eMessage = "Cannot create a user." }
    Just user -> do
      encoded <- (liftAndCatchIO . userIdToToken) $ uId user
      case encoded of
        Left _              -> json $ ErrorResponse { eMessage = "Cannot encode user token." }
        Right encodedUserId -> json $ TokenResponse { uToken = encodedUserId }

login :: ActionM ()
login = do
  text "login"
--   paramToken :: String <- param "token"
--   decoded <- liftAndCatchIO $ tokenToUserId (pack paramToken)
--   case decoded of
--     Left _ -> text "Cannot decode token"
--     Right userId -> text $ integerToText userId

-- helper functions

integerToText :: Integer -> ClassyPrelude.Text
integerToText = pack . show