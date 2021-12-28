{-# LANGUAGE OverloadedStrings #-}

module Server
  ( start
  )
where

import Data.Monoid (mconcat)
import qualified Web.Scotty as S
import Router ( routes )

start :: IO ()
start = do
  S.scotty 3000 routes