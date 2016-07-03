{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (nub)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Directory (getDirectoryContents)
import System.Environment (getEnv)
import Text.Regex (mkRegex, subRegex)
import Text.Regex.Base (matchTest)
import Web.Scotty (scotty, middleware)

import Database (getDBConnection, seedDB)
import Routes (routes)

getShows :: IO [FilePath]
getShows = getEnv "ANIMU_HOME" >>= getDirectoryContents

getNames :: [FilePath] -> [String]
getNames = (=<<) getName
  where
    getName x =
      case (matchTest front x, matchTest back x) of
        (True, True) -> [clean x]
        _ -> mempty
      where
        front = mkRegex "^\\[.*\\] "
        back = mkRegex " - \\d*.*$"
        remove a b = subRegex a b ""
        clean a = remove front $ remove back a

main :: IO ()
main = do
  myShows <- nub . getNames <$> getShows
  conn <- getDBConnection
  _ <- seedDB conn myShows

  scotty 1234 $ do
    middleware $ staticPolicy $ addBase "web/dist"
    routes conn myShows
