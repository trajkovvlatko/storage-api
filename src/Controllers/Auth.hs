module Controllers.Auth 
  ( login
  , register
  ) where
import Web.Scotty (ActionM, text, liftAndCatchIO, param, json)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (Text)
import Models.User (createUser, User (uId))
import ClassyPrelude (unpack, pack, Utf8(encodeUtf8))

register :: ActionM ()
register = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"

  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing   -> json () -- TODO: show an error message here
    Just user -> do
      let userId  = uId user

      encoded <- liftAndCatchIO $ userIdToToken userId
      case encoded of
        Left _ -> text "Cannot encode userId to token"
        Right encodedUserId -> do
          text $ (pack . show) encodedUserId

login :: ActionM ()
login = do
  paramToken :: String <- param "token"
  decoded <- liftAndCatchIO $ tokenToUserId (pack paramToken)
  case decoded of
    Left _ -> text "Cannot decode token"
    Right userId -> text $ integerToText userId

-- helper functions

integerToText :: Integer -> Text
integerToText = pack . show