module Models.ItemType
  ( getAllItemTypes,
    getItemType,
    createItemType,
    updateItemType,
    deleteItemType,
    ItemTypeId,
    ItemTypeLabel,
    ItemType
      ( ItemType,
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

type ItemTypeId = Integer

type ItemTypeLabel = String

data ItemType = ItemType
  { dId :: ItemTypeId,
    dLabel :: ItemTypeLabel
  }
  deriving (Generic)

instance FromRow ItemType where
  fromRow = ItemType <$> field <*> field

instance ToJSON ItemType where
  toEncoding (ItemType id' label) =
    pairs $
      "id" .= id'
        <> "label" .= label

-- queries

getAllItemTypes :: IO [ItemType]
getAllItemTypes = do
  withConn $ \conn -> query conn queryString ()
  where
    queryString = "SELECT id, label FROM item_types;"

getItemType :: ItemTypeId -> IO (Maybe ItemType)
getItemType paramId = do
  withConn $ \conn -> query conn queryString [paramId] >>= resultsToMaybeItemType
  where
    queryString = "SELECT id, label FROM item_types WHERE id = ? LIMIT 1"

createItemType :: ItemTypeLabel -> IO (Maybe ItemType)
createItemType paramLabel = do
  withConn $ \conn -> query conn queryString [paramLabel] >>= resultsToMaybeItemType
  where
    queryString = "INSERT INTO item_types (label) VALUES (?) RETURNING id, label"

updateItemType :: ItemTypeId -> Maybe ItemTypeLabel -> IO (Maybe ItemType)
updateItemType paramId paramLabel = do
  withConn $ \conn -> do
    let updateList = catMaybes [const "label = ?" <$> paramLabel]
        paramList =
          catMaybes
            [ toField <$> paramLabel,
              toField <$> Just paramId
            ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE item_types SET " <> updatesString <> " WHERE id = ? RETURNING id, label"
        selectQueryString = "SELECT id, label FROM item_types WHERE id = ? LIMIT 1"

    resultsToMaybeItemType
      =<< if L.null updateList
        then query conn selectQueryString [paramId]
        else query conn updateQueryString paramList

deleteItemType :: ItemTypeId -> IO (Maybe ItemType)
deleteItemType paramId = do
  withConn $ \conn -> query conn queryString [paramId] >>= resultsToMaybeItemType
  where
    queryString = "DELETE FROM item_types WHERE id = ? RETURNING id, label"

-- helper functions

resultsToMaybeItemType :: [(ItemTypeId, ItemTypeLabel)] -> IO (Maybe ItemType)
resultsToMaybeItemType = \case
  [(resId, resLabel)] ->
    return $
      Just $
        ItemType
          { dId = resId,
            dLabel = resLabel
          }
  _ -> return Nothing
