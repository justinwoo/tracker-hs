{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Database where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.ByteString.Char8 (pack, unpack)
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
prefix = pack . (++) trackerHSPrefix

getDBConnection :: IO Connection
getDBConnection = connect defaultConnectInfo

seedDB :: Connection -> [String] -> IO [Either Reply Bool]
seedDB conn = traverse seedEntry
  where
    seedEntry x = runRedis conn $ setnx (prefix x) "0"

data MyShow = MyShow
  { name :: String
  , count :: String
  }

instance ToJSON MyShow where
  toJSON (MyShow name' count') = object ["name" .= name', "count" .= count']

fetchShowsData :: Connection -> [String] -> IO [MyShow]
fetchShowsData conn xs =
  (<$>) extract <$> getShow `traverse` xs
  where
    getShow x = runRedis conn $ (x,) <$> get (prefix x)
    extract (x, y) =
      MyShow
        { name = x
        , count = case y of
            Right (Just a) -> unpack a
            _ -> "N/A"
        }

data UpdateShow = INCREMENT | DECREMENT

updateShow :: UpdateShow -> Connection -> String -> IO (Either Reply Integer)
updateShow update conn =
  runRedis conn . command . prefix
  where
    command = case update of
      INCREMENT -> incr
      DECREMENT -> decr

incrementShow :: Connection -> String -> IO (Either Reply Integer)
incrementShow = updateShow INCREMENT

decrementShow :: Connection -> String -> IO (Either Reply Integer)
decrementShow = updateShow DECREMENT
