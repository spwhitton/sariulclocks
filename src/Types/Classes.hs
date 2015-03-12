module Types.Classes where

data Grade = GradeFive
           | GradeSix

type Multiplier = Float

          -- grade; class; no. Ss
data Class = Grade  Int    Int
