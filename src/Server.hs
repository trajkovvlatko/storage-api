{-# LANGUAGE OverloadedStrings #-}

module Server
  ( start
  )
where

import Data.Monoid (mconcat)
import qualified Web.Scotty as S
import Router ( routes )
import Configuration.Dotenv (defaultConfig, loadFile)

loadEnv :: IO [(String, String)]
loadEnv = loadFile defaultConfig

start :: IO ()
start = do
  loadEnv
  S.scotty 3000 routes