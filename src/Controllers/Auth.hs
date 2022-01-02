module Controllers.Auth 
  ( login
  , register
  , logout
  ) where
import Web.Scotty (ActionM, text, liftAndCatchIO)
import Lib.Auth (generateToken, userIdToToken, tokenToUserId)
import Data.Text.Lazy ( fromStrict )

login :: ActionM ()
login = do
  let userId = "some user id ---"
  encoded <- liftAndCatchIO $ userIdToToken userId
  decoded <- liftAndCatchIO $ tokenToUserId encoded
  text $ fromStrict decoded

register :: ActionM ()
register = do
  text "register"

logout :: ActionM ()
logout = do
  text "logout"