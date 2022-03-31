module Models.Color
  ( getAllColors,
    getColor,
    createColor,
    updateColor,
    deleteColor,
    ColorId,
    ColorLabel,
    Color
      ( Color,
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
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Lib.Auth (UserId)

type ColorId = Integer

type ColorLabel = String

data Color = Color
  { dId :: ColorId,
    dLabel :: ColorLabel
  }
  deriving (Generic)

instance FromRow Color where
  fromRow = Color <$> field <*> field

instance ToJSON Color where
  toEncoding (Color id' label) =
    pairs $
      "id" .= id'
        <> "label" .= label

-- queries

getAllColors :: IO [Color]
getAllColors = do
  withConn $ \conn -> query conn queryString ()
  where
    queryString = [sql| SELECT id, label FROM colors |]

getColor :: ColorId -> IO (Maybe Color)
getColor paramId = do
  withConn $ \conn -> query conn queryString [paramId] >>= resultsToMaybeColor
  where
    queryString = [sql| SELECT id, label FROM colors WHERE id = ? LIMIT 1 |]

createColor :: ColorLabel -> IO (Maybe Color)
createColor paramLabel = do
  withConn $ \conn -> query conn queryString [paramLabel] >>= resultsToMaybeColor
  where
    queryString = [sql| INSERT INTO colors (label) VALUES (?) RETURNING id, label |]

updateColor :: ColorId -> Maybe ColorLabel -> IO (Maybe Color)
updateColor paramId paramLabel = do
  withConn $ \conn -> do
    let updateList = catMaybes [const [sql| label = ? |] <$> paramLabel]
        paramList =
          catMaybes
            [ toField <$> paramLabel,
              toField <$> Just paramId
            ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE colors SET " <> updatesString <> " WHERE id = ? RETURNING id, label"
        selectQueryString = [sql| SELECT id, label FROM colors WHERE id = ? LIMIT 1 |]

    resultsToMaybeColor
      =<< if L.null updateList
        then query conn selectQueryString [paramId]
        else query conn updateQueryString paramList

deleteColor :: ColorId -> IO (Maybe Color)
deleteColor paramId = do
  withConn $ \conn -> query conn queryString [paramId] >>= resultsToMaybeColor
  where
    queryString = [sql| DELETE FROM colors WHERE id = ? RETURNING id, label |]

-- helper functions

resultsToMaybeColor :: [(ColorId, ColorLabel)] -> IO (Maybe Color)
resultsToMaybeColor = \case
  [(resId, resLabel)] ->
    return $
      Just $
        Color
          { dId = resId,
            dLabel = resLabel
          }
  _ -> return Nothing
