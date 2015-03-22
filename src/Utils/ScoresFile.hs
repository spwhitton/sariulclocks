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
import Control.Monad.SariulClocks
import Control.Monad.Trans (liftIO)

scoresToCSV :: ScoresList -> String
scoresToCSV = unlines . foldr step []
  where
    step (theClass, (Score x y)) theLines =
                          (show theClass ++ "," ++ show x ++ "," ++ show y) : theLines

-- no malformed CSV handling here yet!
scoresFromCSV     :: String -> ScoresList
scoresFromCSV csv = foldr step [] (lines csv)
  where
     step line scores = (theClass, Score (read scoreString) (read timeString)) : scores
      where
        classString:scoreString:timeString:[] = splitOn "," line
        theClass = fromJust $ lookupSariulClass ((read . (:[]) . head) classString) ((read . (:[]) . last) classString)

-- try to read from scores-XX.csv where XX is largest timestamp
readScoresFile :: SariulScoresMonad a => a (Maybe ScoresList)
readScoresFile = do
    curDir <- liftIO getCurrentDirectory
    let dataDir = curDir </> "data"
    filenames <- liftM (reverse . sort . filter isCSV) $ liftIO $ getDirectoryContents dataDir
    if null filenames
       then return Nothing
       else do
        -- let scores = liftM scoresFromCSV $ liftIO $ readFile (dataDir </> head filenames)
        scores <- liftIO $ scoresFromCSV <$> readFile (dataDir </> head filenames)
        putScores scores
        return $ Just scores
  where isCSV path = takeExtension path == ".csv"

-- writes to score-XX.csv where XX is unix timestamp: a simple-minded logging
writeScoresFile :: SariulScoresMonad a => a ()
writeScoresFile = do
    scores <- getScores
    curDir <- liftIO getCurrentDirectory
    let dataDir = curDir </> "data"
    timestamp <- liftM round $ liftIO getPOSIXTime
    let filename = dataDir </> ("scores-" ++ show timestamp ++ ".csv")
    liftIO $ writeFile filename (scoresToCSV scores)
