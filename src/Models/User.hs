{-# OPTIONS_GHC -Wno-missing-fields #-}

module Models.User 
  ( createUser
  , User
    ( User
    , rId
    , rName)
  ) where

import Database (withConn)
import Database.PostgreSQL.Simple (query, FromRow)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs)

data User = User
  { rId         :: Int
  , rName       :: String
  , rPassword   :: String } deriving Generic

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToJSON User where
  toEncoding (User rId rName rPassword) =
    pairs $    "id"   .= rId
            <> "name" .= rName


createUser :: String -> String -> IO (Maybe User)
createUser paramEmail paramPassword = do
  results <- withConn $ \conn -> query conn "INSERT INTO users (email, password) VALUES (?, ?) RETURNING id, email" [paramEmail, paramPassword]
  resultsToMaybeUser results

resultsToMaybeUser :: [(Int, String)] -> IO (Maybe User)
resultsToMaybeUser = \case
  [(resId, resName)] ->
    return $ Just $ User { rId = resId, rName = resName }
  _ ->
    return Nothing