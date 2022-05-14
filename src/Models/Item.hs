module Models.Item
  ( getAllItems,
    getItem,
    createItem,
    updateItem,
    deleteItem,
    basicSearch,
    ItemId,
    ItemName,
    Term,
    Item
      ( Item,
        dId
      ),
  )
where

import qualified ClassyPrelude as CP
import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Database (withConn)
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Lib.Auth (UserId)
import Models.Color (ColorId, ColorLabel)
import Models.Drawer (DrawerId, DrawerLevel, DrawerNote)
import Models.ItemType (ItemTypeId, ItemTypeLabel)
import Models.Room (RoomName)
import Models.StorageUnit (StorageUnitName)

type ItemId = Integer

type ItemName = String

type Term = String

data Item = Item
  { dId :: ItemId,
    dUserId :: UserId,
    dDrawerId :: DrawerId,
    dColorId :: ColorId,
    dItemTypeId :: ItemTypeId,
    dName :: ItemName,
    dColorLabel :: ColorLabel,
    dItemTypeLabel :: ItemTypeLabel
  }
  deriving (Generic)

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Item where
  toEncoding (Item id' userId drawerId colorId itemTypeId name colorLabel itemTypeLabel) =
    pairs $
      "id" .= id'
        <> "user_id" .= userId
        <> "drawer_id" .= drawerId
        <> "color_id" .= colorId
        <> "item_type_id" .= itemTypeId
        <> "name" .= name
        <> "color_label" .= colorLabel
        <> "item_type_label" .= itemTypeLabel

data SearchResult = SearchResult
  { srName :: ItemName,
    srDrawerNote :: DrawerNote,
    srDrawerLevel :: DrawerLevel,
    srStorageUnitName :: StorageUnitName,
    srRoomName :: RoomName,
    srColorLabel :: ColorLabel,
    srItemTypeLabel :: ItemTypeLabel
  }
  deriving (Generic)

instance FromRow SearchResult where
  fromRow = SearchResult <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON SearchResult where
  toEncoding (SearchResult itemName drawerNote drawerLevel storageUnitName roomName colorLabel itemTypeLabel) =
    pairs $
      "name" .= itemName
        <> "drawer_note" .= drawerNote
        <> "drawer_level" .= drawerLevel
        <> "storage_unit_name" .= storageUnitName
        <> "room_name" .= roomName
        <> "color" .= colorLabel
        <> "item_type" .= itemTypeLabel

-- queries

getAllItems :: UserId -> DrawerId -> IO [Item]
getAllItems userId paramDrawerId = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId)
  where
    queryString =
      [sql| SELECT
              items.id, user_id, drawer_id, color_id, item_type_id, name,
              colors.label AS color_label,
              item_types.label AS item_type_label
            FROM items
            JOIN colors ON colors.id = items.color_id
            JOIN item_types ON item_types.id = items.item_type_id
            WHERE user_id = ?
              AND drawer_id = ? |]

getItem :: UserId -> ItemId -> IO (Maybe Item)
getItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString =
      [sql| SELECT
              items.id, user_id, drawer_id, color_id, item_type_id, name,
              colors.label AS color_label,
              item_types.label AS item_type_label
            FROM items
            JOIN colors ON colors.id = items.color_id
            JOIN item_types ON item_types.id = items.item_type_id
            WHERE items.id = ?
              AND user_id = ?
            LIMIT 1 |]

createItem :: UserId -> DrawerId -> ColorId -> ItemTypeId -> ItemName -> IO (Maybe Item)
createItem userId paramDrawerId paramColorId paramItemTypeId paramName = do
  withConn $ \conn -> query conn queryString (userId, paramDrawerId, userId, paramColorId, paramItemTypeId, paramName) >>= resultsToMaybeItem
  where
    queryString =
      [sql| INSERT INTO items (user_id, drawer_id, color_id, item_type_id, name)
            VALUES (
              ?,
              (SELECT id FROM drawers where id = ? AND user_id = ?),
              (SELECT id FROM colors where id = ?),
              (SELECT id FROM item_types where id = ?),
              ?)
            RETURNING id, user_id, drawer_id, color_id, item_type_id, name, '' AS color_label, '' AS item_type_label |]

updateItem :: UserId -> ItemId -> Maybe DrawerId -> Maybe ColorId -> Maybe ItemTypeId -> Maybe ItemName -> IO (Maybe Item)
updateItem userId paramId paramDrawerId paramColorId paramItemTypeId paramName = do
  withConn $ \conn -> do
    let updateList =
          catMaybes
            [ const [sql| drawer_id = ? |] <$> paramDrawerId,
              const [sql| color_id = ? |] <$> paramColorId,
              const [sql| item_type_id = ? |] <$> paramItemTypeId,
              const [sql| name = ? |] <$> paramName
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
        updateQueryString = "UPDATE items SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, user_id, drawer_id, color_id, item_type_id, name, '' AS color_label, '' AS item_type_label"
        selectQueryString =
          [sql|
          SELECT
            items.id, user_id, drawer_id, color_id, item_type_id, name,
            colors.label AS color_label,
            item_types.label AS item_type_label
          FROM items
          JOIN colors ON colors.id = items.color_id
          JOIN item_types ON item_types.id = items.item_type_id
          WHERE items.id = ?
          AND user_id = ? LIMIT 1 |]

    resultsToMaybeItem
      =<< if L.null updateList
        then query conn selectQueryString (paramId, userId)
        else query conn updateQueryString paramList

deleteItem :: UserId -> ItemId -> IO (Maybe Item)
deleteItem userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeItem
  where
    queryString =
      [sql| DELETE FROM items
            WHERE id = ?
              AND user_id = ?
            RETURNING id, user_id, drawer_id, color_id, item_type_id, name, '' AS color_label, '' AS item_type_label |]

basicSearch :: UserId -> Term -> IO [SearchResult]
basicSearch _ "" = return []
basicSearch userId paramTerm = do
  withConn $ \conn -> query conn (CP.fromString queryString) [userId]
  where
    queryArray =
      [ "SELECT items.name, drawers.note AS drawer_note, drawers.level AS drawer_level, storage_units.name AS storage_unit_name, rooms.name AS room_name, colors.label AS color, item_types.label AS item_type",
        "FROM items",
        "JOIN drawers ON drawers.id = items.drawer_id",
        "JOIN storage_units ON storage_units.id = drawers.storage_unit_id",
        "JOIN rooms ON rooms.id = storage_units.room_id",
        "JOIN colors ON colors.id = items.color_id",
        "JOIN item_types ON item_types.id = items.item_type_id",
        "WHERE items.user_id = ?",
        "AND items.name LIKE '%" ++ paramTerm ++ "%'",
        "ORDER BY items.name ASC"
      ]
    queryString = CP.unwords queryArray

-- helper functions

resultsToMaybeItem :: [(ItemId, UserId, DrawerId, ColorId, ItemTypeId, ItemName, ColorLabel, ItemTypeLabel)] -> IO (Maybe Item)
resultsToMaybeItem = \case
  [(resId, resUserId, resDrawerId, resColorId, resItemTypeId, resName, resColorLabel, resItemTypeLabel)] ->
    return $
      Just $
        Item
          { dId = resId,
            dUserId = resUserId,
            dDrawerId = resDrawerId,
            dColorId = resColorId,
            dItemTypeId = resItemTypeId,
            dName = resName,
            dColorLabel = resColorLabel,
            dItemTypeLabel = resItemTypeLabel
          }
  _ -> return Nothing
