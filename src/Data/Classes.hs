module Data.Classes (sariulClasses) where

import Types.Classes

sariulGrade5 = GradeFive 1.5
sariulGrade6 = GradeSix  1

sariulClasses :: [Class]
sariulClasses =
    [ Class sariulGrade5     1    25
    , Class sariulGrade5     2    25
    , Class sariulGrade5     3    25
    , Class sariulGrade6     1    25
    , Class sariulGrade6     2    25
    , Class sariulGrade6     3    25
    , Class sariulGrade6     4    25 ]
