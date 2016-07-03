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
prefix x = pack $ trackerHSPrefix ++ x

getDBConnection :: IO Connection
getDBConnection = connect defaultConnectInfo

seedDB :: Connection -> [String] -> IO [Either Reply Bool]
seedDB conn xs = seedEntry `traverse` xs
  where
    seedEntry x = runRedis conn $ setnx (prefix x) "0"

data MyShow = MyShow
  { name :: String
  , count :: String
  }

instance ToJSON MyShow where
  toJSON (MyShow name' count') = object ["name" .= name', "count" .= count']

extract :: [(String, Either Reply (Maybe ByteString))] -> [MyShow]
extract xs =
  extract' <$> xs
  where
    extract' (x, y) =
      MyShow
        { name = x
        , count = case y of
            Right (Just a) -> unpack a
            _ -> "N/A"
        }

fetchShowsData :: Connection -> [String] -> IO [MyShow]
fetchShowsData conn xs =
  extract <$> getShow `traverse` xs
  where
    getShow x = runRedis conn $ (x,) <$> get (prefix x)

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
