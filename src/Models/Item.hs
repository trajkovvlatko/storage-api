module Models.Item
  ( getAllItems,
    getItem,
    createItem,
    updateItem,
    deleteItem,
    ItemId,
    ItemName,
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
import Models.Color (ColorId)
import Models.Drawer (DrawerId)
import Models.ItemType (ItemTypeId)

type ItemId = Integer

type ItemName = String

data Item = Item
  { dId :: ItemId,
    dUserId :: UserId,
    dDrawerId :: DrawerId,
    dColorId :: ColorId,
    dItemTypeId :: ItemTypeId,
    dName :: ItemName
  }
  deriving (Generic)

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Item where
  toEncoding (Item id' userId drawerId colorId itemTypeId name) =
    pairs $
      "id" .= id'
        <> "user_id" .= userId
        <> "drawer_id" .= drawerId
        <> "color_id" .= colorId
        <> "item_type_id" .= itemTypeId
        <> "name" .= name

-- queries

getAllItems :: UserId -> DrawerId -> IO [Item]
getAllItems userId paramDrawerId = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId)
  where
    queryString = "SELECT id, user_id, drawer_id, color_id, item_type_id, name FROM items WHERE user_id = ? AND drawer_id = ?;"

getItem :: UserId -> ItemId -> IO (Maybe Item)
getItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString = "SELECT id, user_id, drawer_id, color_id, item_type_id, name FROM items WHERE id = ? AND user_id = ? LIMIT 1"

createItem :: UserId -> DrawerId -> ColorId -> ItemTypeId -> ItemName -> IO (Maybe Item)
createItem userId paramDrawerId paramColorId paramItemTypeId paramName = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId, userId, paramColorId, paramItemTypeId, paramName) >>= resultsToMaybeItem
  where
    queryString = "INSERT INTO items (user_id, drawer_id, color_id, item_type_id, name) VALUES (?, (SELECT id FROM drawers where id = ? AND user_id = ?), (SELECT id FROM colors where id = ?), (SELECT id FROM item_types where id = ?), ?) RETURNING id, user_id, drawer_id, color_id, item_type_id, name"

updateItem :: UserId -> ItemId -> Maybe DrawerId -> Maybe ColorId -> Maybe ItemTypeId -> Maybe ItemName -> IO (Maybe Item)
updateItem userId paramId paramDrawerId paramColorId paramItemTypeId paramName = do
  withConn $ \conn -> do
    let updateList =
          catMaybes
            [ const "drawer_id = ?" <$> paramDrawerId,
              const "color_id = ?" <$> paramColorId,
              const "item_type_id = ?" <$> paramItemTypeId,
              const "name = ?" <$> paramName
            ]
        paramList =
          catMaybes
            [ toField <$> paramDrawerId,
              toField <$> paramColorId,
              toField <$> paramItemTypeId,
              toField <$> paramName,
              toField <$> Just paramId,
              toField <$> Just userId
            ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE items SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, user_id, drawer_id, color_id, item_type_id, name"
        selectQueryString = "SELECT id, user_id, drawer_id, color_id, item_type_id, name FROM items WHERE id = ? AND user_id = ? LIMIT 1"

    resultsToMaybeItem
      =<< if L.null updateList
        then query conn selectQueryString (paramId, userId)
        else query conn updateQueryString paramList

deleteItem :: UserId -> ItemId -> IO (Maybe Item)
deleteItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString = "DELETE FROM items WHERE id = ? AND user_id = ? RETURNING id, user_id, drawer_id, color_id, item_type_id, name"

-- helper functions

resultsToMaybeItem :: [(ItemId, UserId, DrawerId, ColorId, ItemTypeId, ItemName)] -> IO (Maybe Item)
resultsToMaybeItem = \case
  [(resId, resUserId, resDrawerId, resColorId, resItemTypeId, resName)] ->
    return $
      Just $
        Item
          { dId = resId,
            dUserId = resUserId,
            dDrawerId = resDrawerId,
            dColorId = resColorId,
            dItemTypeId = resItemTypeId,
            dName = resName
          }
  _ -> return Nothing
