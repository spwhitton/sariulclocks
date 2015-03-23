module Utils.Classes (rankings) where

import Control.Monad.SariulClocks
import Text.XHtml
import Types.Session
import Types.Classes
import Types.Scores
import Utils.Xhtml
import Data.List (sortBy)
import Data.Function (on)
import Text.XHtml.Bootstrap
import Control.Monad (liftM, liftM2)

rankings'                    :: Maybe Class -> ScoresList -> ([Html], [Html], [Html])
rankings' theClass theScores = foldr step ([], [], []) . sortBy (flip $ (compare `on` snd)) $ theScores
  where
    columnsBeforeWinners     = length theScores
    step (thisClass, (Score points timeWasted)) (top, middle, bottom) =
        let cell = if   Just thisClass == theClass
                        then bootstrapCellClass "info"
                        else if   columnsBeforeWinners - 2 <= length top
                             then bootstrapCellClass "success"
                             else td
        in ( (cell << strong << (niceDashes . show) thisClass) : top
           , (cell << show points) : middle
           , ((if timeWasted >= 60 then bootstrapCellClass "danger" else cell) << secondsToTime timeWasted) : bottom )

rankings :: SariulClocksCGI Html
rankings = do
    (top, middle, bottom) <- liftM2 rankings' (liftM currentClass getSession) getScores

    return (thediv # "container" << thediv # "col-md-12" <<
            table # "table table-bordered table-centered table-chunky"
            << ((tr << top) +++ (tr << middle) +++ (tr << bottom)))

secondsToTime         :: Int -> String
secondsToTime n       = minutes' ++ ":" ++ seconds'
  where
    minutes           = n `quot` 60
    seconds           = n `mod`  60
    minutes'          = show minutes
    seconds'
        | seconds > 9 = show seconds
        | otherwise   = '0' : show seconds

-- Makes <td class="contextualClass">.
bootstrapCellClass                 :: String -> (Html -> Html)
bootstrapCellClass contextualClass = td ! [htmlAttr "class" (noHtml +++ contextualClass)]
