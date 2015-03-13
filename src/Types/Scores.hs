module Types.Scores where

          --       points   seconds wasted this week
data Score = Score Int      Int
           deriving (Show, Eq, Ord)

emptyScore = Score 0 0
