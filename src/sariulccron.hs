{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        ((<$>))
import           Control.Monad              (liftM, when)
import           Control.Monad.SariulClocks
import           Control.Monad.Trans        (liftIO)
import           Data.Classes
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Maybe                 (fromJust)
import           System.Directory           (setCurrentDirectory)
import           System.Environment         (getArgs)
import           Text.PrettyPrint.Boxes
import           Types.Classes
import           Types.Scores
import           Utils.ScoresFile

--- meaty functions

weeklyCron        :: ScoresList -> ScoresList
weeklyCron scores = ((resetTime . deductPoints 10) <$> take 3 sortedScores)
                    ++ (resetTime <$> drop 3 sortedScores)
  where
    sortedScores  = sortBy (flip $ (compare `on` (scoreTimeWasted . snd))) scores

scoreTimeWasted             :: Score -> Int
scoreTimeWasted (Score _ x) = x

deductPoints                  :: Int -> (Class, Score) -> (Class, Score)
deductPoints n (c, Score x y) = if   y > 0
                                then (c, Score (x - n) y)
                                else (c, Score x       y)

resetTime                :: (Class, Score) -> (Class, Score)
resetTime (c, Score x _) = (c, Score x 0)

main :: IO ()
main = runSariulClocksIO $ do
    -- TODO: error handling if no cmd line supplied!
    liftIO $ liftM head getArgs >>= setCurrentDirectory
    scores <- readScoresFile
    -- Proceed only if we actually read some scores.
    when (isJust scores) $ do
        modifyScores weeklyCron
        shouldModify <- liftM (((/=) scores) . Just) getScores

        when shouldModify $ do
            writeScoresFile

            -- Output what we did to be e-mailed from crond.
            liftM (scoresBeforeAfter (fromJust scores)) getScores
                >>= printLn

--- utility functions

ppScores      :: ScoresList -> String
ppScores x    = render $
                   hsep 3 center1 [ alignHoriz center2 7  "Class"
                                  , alignHoriz center1 7  "Points"
                                  , alignHoriz center1 12 "Time wasted"]
                // "--------------------------------"
                // foldr step nullBox x
  where
    step b bs = classBox b // bs

classBox               :: (Class, Score) -> Box
classBox (c, (Score x y)) = hsep 3 left
                            [ alignHoriz center1 7  $ (text . show) c
                            , alignHoriz center1 7  $ (text . show) x
                            , alignHoriz center1 12 $ (text . show) y]

scoresBeforeAfter     :: ScoresList -> ScoresList -> String
scoresBeforeAfter x y = "Scores before:\n\n" ++ ppScores x ++ "\nScores after:\n\n" ++ ppScores y

isJust          :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False
