module Types.Classes where

import Types.Scores
import Text.XHtml
import Data.List (sortBy)
import Data.Function (on)

data Grade = GradeFive Multiplier
           | GradeSix  Multiplier
           deriving (Eq, Ord)

type Multiplier = Float

                -- grade; class; no. Ss
data Class = Class Grade  Int    Int
           deriving (Eq)

type ScoresList = [(Class, Score)]

instance Show Grade where
    show (GradeFive _) = "5"
    show (GradeSix  _) = "6"

instance Show Class where
    show (Class g n _) = show g ++ "-" ++ show n

numberOfSs               :: Class -> Int
numberOfSs (Class _ _ n) = n

-- rankings        :: ScoresList -> Html
-- rankings scores = table << tr << (foldr step [] scores)
--   where
--     step x xs = undefined

rankingColumns        :: Maybe Class -> ScoresList -> [(Html, Html, Html)]
rankingColumns currentClass scores = foldr step [] sortedScores
  where
    sortedScores = reverse . sortBy (compare `on` snd) $ scores
    step (thisClass, (Score points timeWasted)) cols =
        ( (if   Just thisClass == currentClass
           then bootstrapCellClass ".info"
           else td)
          << h2 << show thisClass
        , td << h3 << show points
        , (if  timeWasted >= 60
           then bootstrapCellClass ".warning"
           else td)
          <<  show timeWasted
        ) : cols
    maybeBorder aClass=
        if   Just aClass == currentClass
        then [htmlAttr "class" noHtml]
        else undefined

bootstrapCellClass                 :: String -> (Html -> Html)
bootstrapCellClass contextualClass = td ! [htmlAttr "class" (noHtml +++ contextualClass)]
