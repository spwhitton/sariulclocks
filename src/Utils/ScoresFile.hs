module Utils.ScoresFile ( readScoresFile
                        , writeScoresFile) where

import Types.Scores
import Types.Classes
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import System.IO (withFile)

scoresToCSV :: ScoresList -> String
scoresToCSV = undefined

scoresFromCSV :: String -> ScoresList
scoresFromCSV = undefined

-- read to scores-XX.csv where XX is largest timestamp
readScoresFile :: IO ScoresList
readScoresFile = undefined

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: ScoresList -> IO ()
writeScoresFile scores = do
    timestamp <- round <$> getPOSIXTime
    let filename = "scores-" ++ show timestamp ++ ".csv"
    writeFile filename (scoresToCSV scores)
