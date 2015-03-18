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

getModifier                           :: Class -> Float
getModifier (Class (GradeFive m) _ _) = m
getModifier (Class (GradeSix  m) _ _) = m

updateScore          :: ScoresList -> Class -> Int -> Int -> ScoresList
updateScore [] _ _ _ = []
updateScore (s@(aClass, Score x y):ss) c p t
    | c == aClass    = (c, Score (x + (floor $ (fromIntegral p) * (getModifier aClass))) (y + t)):ss
    | otherwise      = s:updateScore ss c p t
