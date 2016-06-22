{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (ByteString)
import Database.Redis
  ( Connection
  , Reply
  , connect
  , defaultConnectInfo
  , runRedis
  , get
  , setnx
  , incr
  , decr
  )

trackerHSPrefix :: String
trackerHSPrefix = "TRACKERHS-PREFIX-"

prefix :: String -> ByteString
prefix x = pack $ trackerHSPrefix ++ x

getDBConnection :: IO Connection
getDBConnection = connect defaultConnectInfo

seedDB :: Connection -> [String] -> IO [Either Reply Bool]
seedDB conn xs = seedEntry `traverse` xs
  where
    seedEntry x = runRedis conn $ setnx (prefix x) "0"

fetchShowsData :: Connection -> [String] -> IO [Either Reply (Maybe ByteString)]
fetchShowsData conn xs =
  getShow `traverse` xs
  where
    getShow x = runRedis conn $ get (prefix x)

data UpdateShow = INCREMENT | DECREMENT

updateShow :: UpdateShow -> Connection -> String -> IO (Either Reply Integer)
updateShow update conn x =
  runRedis conn $ command (prefix x)
  where
    command = case update of
      INCREMENT -> incr
      DECREMENT -> decr

incrementShow :: Connection -> String -> IO (Either Reply Integer)
incrementShow = updateShow INCREMENT

decrementShow :: Connection -> String -> IO (Either Reply Integer)
decrementShow = updateShow DECREMENT
