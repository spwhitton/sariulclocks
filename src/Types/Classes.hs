module Types.Classes where

data Grade = GradeFive
           | GradeSix

type Multiplier = Float

                -- grade; class; no. Ss
data Class = Class Grade  Int    Int
