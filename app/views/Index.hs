{-# LANGUAGE OverloadedStrings #-}

module Views.Index (indexView) where

import Prelude hiding (div, head, id)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
  ( (!)
  , toHtml
  , div
  , script
  )
import Text.Blaze.Html5.Attributes
  ( class_
  , id
  , src
  )
import Text.Printf (printf)
import Web.Scotty (ActionM, html)

import Views.Layout (layout)

injectScript :: String -> Html
injectScript x = script $ toHtml x

indexView :: String -> ActionM ()
indexView showData = html . renderHtml $ layout "tracker" $ do
  div ! class_ "container-fluid" $
    div ! id "app" $ mempty
  injectScript $ printf "showData = JSON.parse(%s);" showData
  script ! src "index.js" $ mempty
