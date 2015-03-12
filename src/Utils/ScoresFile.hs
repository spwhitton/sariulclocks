module Utils.ScoresFile ( readScoresFile
                        , writeScoresFile) where

import Types.Scores
import Types.Classes
import System.Posix.Files

-- read to scores-XX.csv where XX is largest timestamp
readScoresFile :: IO [(Class,Score)]
readScoresFile = undefined

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: [(Class,Score)] -> IO ()
writeScoresFile = undefined
