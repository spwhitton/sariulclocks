module Text.XHtml.Bootstrap where

import Text.XHtml

(#)          :: ADDATTRS a => a -> String -> a
e # theClass = e ! [strAttr "class" theClass]

(#=)          :: ADDATTRS a => a -> String -> a
e #= theClass = e ! [strAttr "id" theClass]

bsButton                   :: HTML a => String -> String -> a -> Html
bsButton id theClass label = anchor # theClass #= id
                             << label
