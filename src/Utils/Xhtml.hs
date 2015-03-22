module Utils.Xhtml ( niceDashes
                   , uC) where

import Text.XHtml

-- Very simple atm.  Not easily extended to handle emdashes too.

niceDashes      :: String -> String
niceDashes []   = []
niceDashes (x:xs)
    | x == '-'  = "–" ++ niceDashes xs
    | otherwise = x : niceDashes xs

uC   :: Char -> Html
uC c = thespan ! [strAttr "style" "text-decoration: underline;"] << [c]
