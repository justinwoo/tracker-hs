{-# LANGUAGE OverloadedStrings #-}

module Views.Layout (layout) where

import Prelude hiding (div, head, id)
import Text.Blaze.Html (Html)
import Text.Blaze.Html5
  ( (!)
  , docTypeHtml
  , head
  , body
  , meta
  , title
  , link
  )
import Text.Blaze.Html5.Attributes
  ( charset
  , href
  , rel
  )

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
  head $ do
    title t
    meta ! charset "utf-8"
    link ! href "//v4-alpha.getbootstrap.com/dist/css/bootstrap.min.css" ! rel "stylesheet"
  body b
