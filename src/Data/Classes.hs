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

lookupSariulClass                :: Int -> Int -> Maybe Class
lookupSariulClass grade theClass = do
    grade' <- case grade of
        5 -> Just sariulGrade5
        6 -> Just sariulGrade6
        _ -> Nothing
    let choose (Class g n _) = theClass == n && grade' == g
        classes = filter choose sariulClasses
    case classes of
        []     -> Nothing
        (x:xs) -> Just x
