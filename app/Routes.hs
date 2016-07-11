{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Data.Aeson (encode)
import Data.Text.Lazy (pack)
import Database.Redis (Connection)
import Web.Scotty
  ( ScottyM
  , get
  , post
  , param
  , html
  )

import Database (fetchShowsData, incrementShow, decrementShow)
import Views.Index (indexView)

routes :: Connection -> [String] -> ScottyM ()
routes conn myShows = do
  get "/" $ do
    myShows' <- show . encode <$> fetchShowsData conn myShows
    indexView myShows'

  post "/increment" $ do
    name <- param "name"
    result <- incrementShow conn name
    case result of
      Right int -> html $ pack $ show int
      _ -> html "u suck"

  post "/decrement" $ do
    name <- param "name"
    result <- decrementShow conn name
    case result of
      Right int -> html $ pack $ show int
      _ -> html "u suck"
