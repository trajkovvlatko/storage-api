module Main where

import ClassyPrelude (fromString)
import qualified Controllers.AuthSpec
import qualified Controllers.ColorsSpec
import qualified Controllers.DrawersSpec
import qualified Controllers.ItemTypesSpec
import qualified Controllers.RoomsSpec
import qualified Controllers.StorageUnitsSpec
import Database (withConn)
import Database.PostgreSQL.Simple (execute_)
import qualified Database.PostgreSQL.Simple.Types
import Server (loadEnv)
import Test.Hspec

truncateQuery :: Database.PostgreSQL.Simple.Types.Query
truncateQuery = fromString $ concatMap (\t -> "SET client_min_messages = WARNING; TRUNCATE TABLE " ++ t ++ " CASCADE;") tables
  where
    tables =
      [ "item_types",
        "colors",
        "users",
        "rooms",
        "storage_units",
        "drawers"
      ]

initSpecs :: IO ()
initSpecs = do
  _ <- loadEnv
  withConn (`execute_` truncateQuery)
  return ()

main :: IO ()
main = hspec $ before_ initSpecs spec

spec :: Spec
spec = do
  describe "Auth" Controllers.AuthSpec.spec
  describe "Rooms" Controllers.RoomsSpec.spec
  describe "StorageUnits" Controllers.StorageUnitsSpec.spec
  describe "Drawers" Controllers.DrawersSpec.spec
  describe "ItemTypes" Controllers.ItemTypesSpec.spec
  describe "Colors" Controllers.ColorsSpec.spec
