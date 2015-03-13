module Types.Classes where

import Types.Scores

data Grade = GradeFive Multiplier
           | GradeSix  Multiplier

type Multiplier = Float

                -- grade; class; no. Ss
data Class = Class Grade  Int    Int

type ScoresList = [(Grade, Score)]

instance Show Grade where
    show (GradeFive _) = "5"
    show (GradeSix  _) = "6"

instance Show Class where
    show (Class g n _) = show g ++ "-" ++ show n

numberOfSs               :: Class -> Int
numberOfSs (Class _ _ n) = n
