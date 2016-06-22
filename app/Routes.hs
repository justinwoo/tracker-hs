{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes (routes) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Internal (ByteString)
import Data.List (intercalate)
import Data.Text.Lazy (pack)
import Database.Redis (Connection, Reply)
import Text.Printf (printf)
import Web.Scotty
  ( ScottyM
  , get
  , post
  , param
  , html
  )

import Database (fetchShowsData, incrementShow, decrementShow)
import Views.Index (indexView)

extract :: Either Reply (Maybe ByteString) -> String
extract x = case x of
  Right (Just a) -> unpack a
  _ -> "N/A"

makeShowData :: (String, String) -> String
makeShowData (name, count) =
  printf "{name: \"%s\", count: \"%s\"}" name count

routes :: Connection -> [String] -> ScottyM ()
routes conn myShows = do
  get "/" $ do
    results <- liftIO $ fetchShowsData conn myShows
    let showData =
          printf "[%s]" $
          intercalate "," $
          makeShowData <$> zip myShows (extract <$> results)
    indexView showData

  post "/increment" $ do
    name :: String <- param "name"
    result <- liftIO $ incrementShow conn name
    case result of
      Right int -> html $ pack $ show int
      _ -> html "u suck"

  post "/decrement" $ do
    name :: String <- param "name"
    result <- liftIO $ decrementShow conn name
    case result of
      Right int -> html $ pack $ show int
      _ -> html "u suck"
