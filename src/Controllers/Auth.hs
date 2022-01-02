module Controllers.Auth 
  ( login
  , register
  , logout
  ) where
import Web.Scotty (ActionM, text, liftAndCatchIO)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (pack)

login :: ActionM ()
login = do
  let userId = 1234
  encoded <- liftAndCatchIO $ userIdToToken userId
  decoded <- liftAndCatchIO $ tokenToUserId encoded
  text $ (pack . show) decoded

register :: ActionM ()
register = do
  text "register"

logout :: ActionM ()
logout = do
  text "logout"