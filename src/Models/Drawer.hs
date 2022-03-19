module Models.Drawer 
  ( getAllDrawers
  , getDrawer
  , createDrawer
  , updateDrawer
  , deleteDrawer
  , Drawer
    ( Drawer
    , dId
    , dLevel
    , dNote)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toEncoding), KeyValue((.=)), pairs)
import Lib.Auth (UserId)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Simple.ToField (ToField(toField))

data Drawer = Drawer
  { dId        :: Integer
  , dStorageUnitId :: Integer
  , dLevel     :: Integer
  , dNote      :: String } deriving Generic

instance FromRow Drawer where
  fromRow = Drawer <$> field <*> field <*> field <*> field

instance ToJSON Drawer where
  toEncoding (Drawer id' storageUnitId level note) =
    pairs $    "id"              .= id'
            <> "storage_unit_id" .= storageUnitId
            <> "level"           .= level
            <> "note"            .= note

-- queries

getAllDrawers :: UserId -> Integer -> IO [Drawer]
getAllDrawers userId paramStorageUnitId = do
  withConn $ \conn -> query conn queryString (userId, paramStorageUnitId)
  where queryString = "SELECT id, storage_unit_id, level, note FROM drawers WHERE user_id = ? AND storage_unit_id = ?;"

getDrawer :: UserId -> Integer -> IO (Maybe Drawer)
getDrawer userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeDrawer
  where queryString = "SELECT id, storage_unit_id, level, note FROM drawers WHERE id = ? AND user_id = ? LIMIT 1"

createDrawer :: UserId -> Integer -> Integer -> String -> IO (Maybe Drawer)
createDrawer userId paramStorageUnitId paramLevel paramNote = do
  withConn $ \conn -> query conn queryString (userId, paramStorageUnitId, userId, paramLevel, paramNote) >>= resultsToMaybeDrawer
  where queryString = "INSERT INTO drawers (user_id, storage_unit_id, level, note) VALUES (?, (SELECT id FROM storage_units where id = ? AND user_id = ?), ?, ?) RETURNING id, storage_unit_id, level, note"

updateDrawer :: UserId -> Integer -> Maybe Integer -> Maybe Integer -> Maybe String -> IO (Maybe Drawer)
updateDrawer userId paramId paramStorageUnitId paramLevel paramNote = do
  withConn $ \conn -> do
    let updateList = catMaybes
          [ const "storage_unit_id = ?" <$> paramStorageUnitId
          , const "level = ?" <$> paramLevel
          , const "note = ?" <$> paramNote ]
        paramList = catMaybes
          [ toField <$> paramStorageUnitId
          , toField <$> paramLevel
          , toField <$> paramNote
          , toField <$> Just paramId
          , toField <$> Just userId ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE drawers SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, storage_unit_id, level, note"
        selectQueryString = "SELECT id, storage_unit_id, level, note FROM drawers WHERE id = ? AND user_id = ? LIMIT 1"

    resultsToMaybeDrawer =<< if L.null updateList
      then query conn selectQueryString (paramId, userId)
      else query conn updateQueryString paramList

deleteDrawer :: UserId -> Integer -> IO (Maybe Drawer)
deleteDrawer userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeDrawer
  where queryString = "DELETE FROM drawers WHERE id = ? AND user_id = ? RETURNING id, storage_unit_id, level, note"

-- helper functions

resultsToMaybeDrawer :: [(Integer, Integer, Integer, String)] -> IO (Maybe Drawer)
resultsToMaybeDrawer = \case
  [(resId, resStorageUnitId, resLevel, resNote)] -> return $ Just $ Drawer { dId = resId
                                                                           , dStorageUnitId = resStorageUnitId
                                                                           , dLevel = resLevel
                                                                           , dNote = resNote }
  _ -> return Nothing
