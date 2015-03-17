module Types.Classes where

import Types.Scores
import Text.XHtml.Strict
import Data.List (sortBy)
import Data.Function (on)
import Utils.Xhtml (niceDashes)

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
