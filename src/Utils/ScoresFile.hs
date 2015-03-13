module Utils.ScoresFile ( readScoresFile
                        , writeScoresFile) where

import Types.Scores
import Types.Classes
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import System.IO (readFile, writeFile)
import Data.List (sort)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.FilePath (takeExtension)
import Control.Monad (liftM)

scoresToCSV :: ScoresList -> String
scoresToCSV = foldr undefined undefined

scoresFromCSV     :: String -> ScoresList
scoresFromCSV csv = foldr undefined undefined (lines csv)

-- read to scores-XX.csv where XX is largest timestamp
readScoresFile :: IO ScoresList
readScoresFile = do
    curDir <- getCurrentDirectory
    -- what if there's no CSV file yet?  head will throw an exception.  handle it.
    filename <- liftM (head . reverse . sort . filter csvs) $ getDirectoryContents curDir
    scoresFromCSV <$> readFile filename
  where csvs path = takeExtension path == ".csv"

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: ScoresList -> IO ()
writeScoresFile scores = do
    timestamp <- round <$> getPOSIXTime
    let filename = "scores-" ++ show timestamp ++ ".csv"
    writeFile filename (scoresToCSV scores)
