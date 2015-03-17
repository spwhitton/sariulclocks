module Text.XHtml.Bootstrap where

import Text.XHtml

button                     :: String -> String -> String -> Html
button id theClass label = anchor ! [ strAttr "id" id
                                    , strAttr "class" theClass]
                           << label
