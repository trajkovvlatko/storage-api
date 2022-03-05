module Main where

import Test.Hspec
import Database (withConn)
import Database.PostgreSQL.Simple (execute_)

import qualified Controllers.RoomsSpec
import Server (loadEnv)
import qualified Database.PostgreSQL.Simple.Types
import ClassyPrelude (fromString)

truncateQuery :: Database.PostgreSQL.Simple.Types.Query
truncateQuery = fromString $ concatMap (\t -> "TRUNCATE TABLE " ++ t ++ "; ") tables
  where tables = [ "users"
                 , "rooms"
                 , "storage_units"
                 , "drawers" ]

initSpecs :: IO ()
initSpecs = do
  _ <- loadEnv
  withConn (`execute_` truncateQuery)
  return ()

main :: IO ()
main = hspec $ before_ initSpecs spec

spec :: Spec
spec = do
  describe "Rooms" Controllers.RoomsSpec.spec
