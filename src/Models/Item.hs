module Models.Item
  ( getAllItems,
    getItem,
    createItem,
    updateItem,
    deleteItem,
    ItemId,
    Item
      ( Item,
        dId
      ),
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Database (withConn)
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Lib.Auth (UserId)
import Models.Drawer (DrawerId)

type ItemId = Integer

data Item = Item
  { dId :: ItemId,
    dUserId :: UserId,
    dDrawerId :: DrawerId
  }
  deriving (Generic)

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field

instance ToJSON Item where
  toEncoding (Item id' userId drawerId) =
    pairs $
      "id" .= id'
        <> "user_id" .= userId
        <> "drawer_id" .= drawerId

-- queries

getAllItems :: UserId -> DrawerId -> IO [Item]
getAllItems userId paramDrawerId = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId)
  where
    queryString = "SELECT id, user_id, drawer_id FROM items WHERE user_id = ? AND drawer_id = ?;"

getItem :: UserId -> ItemId -> IO (Maybe Item)
getItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString = "SELECT id, user_id, drawer_id FROM items WHERE id = ? AND user_id = ? LIMIT 1"

createItem :: UserId -> DrawerId -> IO (Maybe Item)
createItem userId paramDrawerId = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId, userId) >>= resultsToMaybeItem
  where
    queryString = "INSERT INTO items (user_id, drawer_id) VALUES (?, (SELECT id FROM drawers where id = ? AND user_id = ?), ?, ?) RETURNING id, user_id, drawer_id"

updateItem :: UserId -> ItemId -> Maybe DrawerId -> IO (Maybe Item)
updateItem userId paramId paramDrawerId = do
  withConn $ \conn -> do
    let updateList =
          catMaybes
            [ const "drawer_id = ?" <$> paramDrawerId
            ]
        paramList =
          catMaybes
            [ toField <$> paramDrawerId,
              toField <$> Just paramId,
              toField <$> Just userId
            ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE items SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, user_id, drawer_id"
        selectQueryString = "SELECT id, user_id, drawer_id FROM items WHERE id = ? AND user_id = ? LIMIT 1"

    resultsToMaybeItem
      =<< if L.null updateList
        then query conn selectQueryString (paramId, userId)
        else query conn updateQueryString paramList

deleteItem :: UserId -> ItemId -> IO (Maybe Item)
deleteItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString = "DELETE FROM items WHERE id = ? AND user_id = ? RETURNING id, user_id, drawer_id"

-- helper functions

resultsToMaybeItem :: [(ItemId, UserId, DrawerId)] -> IO (Maybe Item)
resultsToMaybeItem = \case
  [(resId, resUserId, resDrawerId)] ->
    return $
      Just $
        Item
          { dId = resId,
            dUserId = resUserId,
            dDrawerId = resDrawerId
          }
  _ -> return Nothing
