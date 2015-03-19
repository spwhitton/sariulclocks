module Utils.ScoresFile ( readScoresFile
                        , writeScoresFile) where

import Types.Scores
import Types.Classes
import Data.Classes
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import System.IO (readFile, writeFile)
import Data.List (sort)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.FilePath (takeExtension)
import Control.Monad (liftM)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.FilePath ((</>))

scoresToCSV :: ScoresList -> String
scoresToCSV = unlines . foldr step []
  where
    step (theClass, (Score x y)) theLines =
                          (show theClass ++ "," ++ show x ++ "," ++ show y) : theLines

-- TODO: back these up

-- no malformed CSV handling here yet!
-- this function currently doesn't work
scoresFromCSV     :: String -> ScoresList
scoresFromCSV csv = foldr step [] (lines csv)
  where
     step line scores = (theClass, Score (read scoreString) (read timeString)) : scores
      where
        classString:scoreString:timeString:[] = splitOn "," line
        theClass = fromJust $ lookupSariulClass ((read . (:[]) . head) classString) ((read . (:[]) . last) classString)

-- read from scores-XX.csv where XX is largest timestamp
readScoresFile :: IO (Maybe ScoresList)
readScoresFile = do
    curDir <- getCurrentDirectory
    let dataDir = curDir </> "data"
    filenames <- liftM (reverse . sort . filter isCSV) $ getDirectoryContents dataDir
    case filenames of
        [] -> return Nothing
        _  -> Just . scoresFromCSV <$> readFile (dataDir </> head filenames)
  where isCSV path = takeExtension path == ".csv"

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: ScoresList -> IO ()
writeScoresFile scores = do
    curDir <- getCurrentDirectory
    let dataDir = curDir </> "data"
    timestamp <- round <$> getPOSIXTime
    let filename = dataDir </> ("scores-" ++ show timestamp ++ ".csv")
    writeFile filename (scoresToCSV scores)
