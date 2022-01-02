module Controllers.Auth 
  ( login
  , register
  , logout
  ) where
import Web.Scotty (ActionM, text, liftAndCatchIO)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (pack, Text)

login :: ActionM ()
login = do
  encoded <- liftAndCatchIO $ userIdToToken 1234
  case encoded of
    Left _ -> text "Cannot encode userId to token"
    Right encodedUserId -> do
      decoded <- liftAndCatchIO $ tokenToUserId encodedUserId
      case decoded of
        Left _ -> text "Cannot decode token"
        Right userId -> text $ integerToText userId

register :: ActionM ()
register = do
  text "register"

logout :: ActionM ()
logout = do
  text "logout"

-- helper functions

integerToText :: Integer -> Text
integerToText = pack . show