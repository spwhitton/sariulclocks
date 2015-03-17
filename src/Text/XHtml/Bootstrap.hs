module Text.XHtml.Bootstrap where

import Text.XHtml

-- # ought to be for Id, something else for class ...

(#)          :: ADDATTRS a => a -> String -> a
e # theClass = e ! [strAttr "class" theClass]

bsButton                   :: HTML a => String -> String -> a -> Html
bsButton id theClass label = anchor ! [ strAttr "id" id
                                    , strAttr "class" theClass]
                             << label
