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
scoresToCSV = unlines . foldr step []
  where
    step (theClass, (Score x y)) theLines =
                          (show theClass ++ "," ++ show x ++ "," ++ show y) : theLines

scoresFromCSV     :: String -> ScoresList
scoresFromCSV csv = foldr undefined [] (lines csv)

-- read to scores-XX.csv where XX is largest timestamp
readScoresFile :: IO (Maybe ScoresList)
readScoresFile = do
    curDir <- getCurrentDirectory
    filenames <- liftM (reverse . sort . filter isCSV) $ getDirectoryContents curDir
    case filenames of
        [] -> return Nothing
        _  -> Just . scoresFromCSV <$> readFile (head filenames)
  where isCSV path = takeExtension path == ".csv"

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: ScoresList -> IO ()
writeScoresFile scores = do
    timestamp <- round <$> getPOSIXTime
    let filename = "scores-" ++ show timestamp ++ ".csv"
    writeFile filename (scoresToCSV scores)
