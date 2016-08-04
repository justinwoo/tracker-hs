{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Internal (ByteString)
import Database.Redis
  ( Connection
  , Reply
  , ConnectInfo
  , PortID(Service)
  , connect
  , connectHost
  , connectPort
  , connectAuth
  , defaultConnectInfo
  , runRedis
  , mget
  , setnx
  , incr
  , decr
  )

trackerHSPrefix :: String
trackerHSPrefix = "TRACKERHS-PREFIX-"

prefix :: String -> ByteString
prefix = pack . (++) trackerHSPrefix

connectInfo :: String -> String -> ByteString -> ConnectInfo
connectInfo host port auth =
 defaultConnectInfo
    { connectHost = host
    , connectPort = Service port
    , connectAuth = Just auth
    }

getDBConnection :: ConnectInfo -> IO Connection
getDBConnection = connect

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

fetchShowsData :: MonadIO m => Connection -> [String] -> m [MyShow]
fetchShowsData conn keys =
  liftIO . runRedis conn $ do
    result <- mget $ map prefix keys
    case result of
      Right xs -> do
        let values = map (maybe "N/A" unpack) xs
        return $ zipWith myShow keys values
      Left _ -> return []
  where
    myShow x y =
      MyShow
        { name = x
        , count = y
        }

data UpdateShow = INCREMENT | DECREMENT

updateShow :: MonadIO m => UpdateShow -> Connection -> String -> m (Either Reply Integer)
updateShow update conn =
  liftIO . runRedis conn . command . prefix
  where
    command = case update of
      INCREMENT -> incr
      DECREMENT -> decr

incrementShow :: MonadIO m => Connection -> String -> m (Either Reply Integer)
incrementShow = updateShow INCREMENT

decrementShow :: MonadIO m => Connection -> String -> m (Either Reply Integer)
decrementShow = updateShow DECREMENT
