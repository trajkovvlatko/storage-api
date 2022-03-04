module Database where

import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
    ( defaultConnectInfo
    , connect
    , ConnectInfo
      ( connectUser
      , connectPassword
      , connectDatabase
      , connectHost
      )
    , Connection
    , close)
import Data.Pool ( createPool, withResource )
import qualified System.Environment as ENV

data DbConfig = DbConfig { dbName     :: String
                         , dbUser     :: String
                         , dbPassword :: String
                         , dbHost     :: String
                         } deriving (Show, Generic)

getDatabaseConfig :: IO DbConfig
getDatabaseConfig = do
  name <- ENV.getEnv "PG_DB"
  user <- ENV.getEnv "PG_USER"
  password <- ENV.getEnv "PG_PASS"
  host <- ENV.getEnv "PG_HOST"
  return $ DbConfig name user password host

getConnection :: DbConfig -> IO Connection
getConnection conf = connect defaultConnectInfo { connectUser     = dbUser conf
                                                , connectPassword = dbPassword conf
                                                , connectDatabase = dbName conf
                                                , connectHost     = dbHost conf
                                                }

withConn :: (Connection -> IO a) -> IO a
withConn action = do
  pool <- createPool (getConnection =<< getDatabaseConfig) close 1 10 10
  withResource pool action
