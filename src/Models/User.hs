module Models.User
  ( createUser,
    findUserByEmail,
    User,
    uId,
    uEmail,
    uPassword,
    Email,
    Password,
  )
where

import ClassyPrelude (IsString (fromString), Text, unpack)
import Control.Exception (try)
import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Data.Password.Bcrypt (Bcrypt, PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Database (withConn)
import Database.PostgreSQL.Simple (FromRow, SqlError, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Lib.Auth (UserId)

type Email = String

type Password = String

data User = User
  { uId :: UserId,
    uEmail :: Email,
    uPassword :: Password
  }
  deriving (Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToJSON User where
  toEncoding (User id' email _) =
    pairs $
      "id" .= id'
        <> "email" .= email

getPasswordHash :: Password -> IO (PasswordHash Bcrypt)
getPasswordHash password = hashPassword (mkPassword (fromString password))

createUser :: Email -> Password -> IO (Maybe User)
createUser paramEmail paramPassword = do
  passwordHash <- getPasswordHash paramPassword
  let passwordString = unpack $ unPasswordHash passwordHash
  resultsToMaybeUser =<< withConn (\conn -> try $ query conn queryString [paramEmail, passwordString])
  where
    queryString = "INSERT INTO users (email, password) VALUES (?, ?) RETURNING id, email, password"

findUserByEmail :: Email -> IO (Maybe User)
findUserByEmail paramEmail = do
  resultsToMaybeUser =<< withConn (\conn -> try $ query conn queryString [paramEmail])
  where
    queryString = "SELECT id, email, password FROM users WHERE email = ?"

-- helper functions

resultsToMaybeUser :: Either SqlError [(UserId, Email, Password)] -> IO (Maybe User)
resultsToMaybeUser maybeUser = do
  case maybeUser of
    Left err -> do
      print err
      return Nothing
    Right [(resId, resEmail, resPassword)] -> do
      return $ Just $ User {uId = resId, uEmail = resEmail, uPassword = resPassword}
    Right invalid -> do
      print invalid
      return Nothing
