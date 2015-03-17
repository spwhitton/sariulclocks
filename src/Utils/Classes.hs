module Utils.Classes (rankings) where

import Control.Monad.Page
import Text.XHtml
import Types.Session
import Types.Classes
import Types.Scores
import Utils.Xhtml
import Data.List (sortBy)
import Data.Function (on)
import Text.XHtml.Bootstrap

-- Make the columns with rankingColumns, and then transpose them so
-- that we can make HTML.

-- Do this as two composed folds (point-free), rather than three:
-- first one accumulates to list of three rows (each step adds to all
-- three rows) and second turns list of three rows into Html.
--
-- This also means that we can have the whole column with a meaningful
-- blue background colour for the current class and the winning two
-- classes, not only the first row.

rankings :: Page Html
rankings = do
    session <- getSession
    scores <- getScores
    return (thediv # "container" << thediv # "col-md-12" <<
        table ! [htmlAttr "class" (noHtml +++ "table table-bordered table-centered table-chunky")]
            << foldr (\row acc -> (tr << row) +++ acc) noHtml (rankings' (currentClass session) scores))

-- rankings                     :: Maybe Class -> ScoresList -> Html
-- rankings currentClass scores = table ! [htmlAttr "class" (noHtml +++ "table table-bordered table-centered table-chunky")]
--                                << foldr (\row acc -> (tr << row) +++ acc) noHtml (rankings' currentClass scores)

rankings'                     :: Maybe Class -> ScoresList -> [Html]
rankings' currentClass scores = foldr step [noHtml, noHtml, noHtml] (rankingColumns currentClass scores)
  where
    step (first, second, third) [firstRow, secondRow, thirdRow] =
        [first +++ firstRow, second +++ secondRow, third +++ thirdRow]

rankingColumns        :: Maybe Class -> ScoresList -> [(Html, Html, Html)]
rankingColumns currentClass scores = fst $ foldr step ([], length sortedScores) sortedScores
  where
    sortedScores = reverse . sortBy (compare `on` snd) $ scores
    step (thisClass, (Score points timeWasted)) (cols, count) =
        (( (if count <= 2
            then bootstrapCellClass "success"
            else if   Just thisClass == currentClass
                 then bootstrapCellClass "info"
                 else bootstrapCellClass "warning")
           << strong << (niceDashes . show) thisClass
         , td << show points
         , (if  timeWasted >= 60
            then bootstrapCellClass "warning"
            else td)
           << secondsToTime timeWasted
         ) : cols, count - 1)
    maybeBorder aClass=
        if   Just aClass == currentClass
        then [htmlAttr "class" noHtml]
        else undefined

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