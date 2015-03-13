{-# LANGUAGE TupleSections #-}

module Data.Classes ( zeroScores
                    , lookupSariulClass) where

import Types.Classes
import Types.Scores

sariulGrade5 = GradeFive 1.5
sariulGrade6 = GradeSix  1

sariulClasses :: [Class]
sariulClasses =
    [ Class sariulGrade5     1    25
    , Class sariulGrade5     2    24
    , Class sariulGrade5     3    25
    , Class sariulGrade6     1    23
    , Class sariulGrade6     2    23
    , Class sariulGrade6     3    23
    , Class sariulGrade6     4    21 ]

zeroScores :: ScoresList
zeroScores = foldr ((:) . (,emptyScore)) [] sariulClasses

lookupSariulClass                :: Int -> Int -> Class
lookupSariulClass grade theClass = (head . filter choose) sariulClasses
  where
    choose (Class g n _) = theClass == n && process grade == g
    process 5 = sariulGrade5
    process 6 = sariulGrade6
