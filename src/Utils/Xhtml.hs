module Utils.Xhtml (niceDashes) where

-- Very simple atm.  Not easily extended to handle emdashes too.

niceDashes      :: String -> String
niceDashes []   = []
niceDashes (x:xs)
    | x == '-'  = "–" ++ niceDashes xs
    | otherwise = x : niceDashes xs
