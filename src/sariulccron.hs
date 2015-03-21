-- import Control.Monad.SariulClocks
import Utils.ScoresFile
import Types.Scores
import Types.Classes
import Control.Monad (liftM, when)
import Text.PrettyPrint.Boxes

scoresBox :: ScoresList -> Box
scoresBox = undefined

weeklyCron        :: ScoresList -> ScoresList
weeklyCron scores = undefined

-- main :: IO ()
-- main = runSariulClocksIO $ do
--     scores <- readScoresFile
--     when (isJust scores) $ do
--         modifyScores weeklyCron
--         shouldModify <- liftM (((/=) scores) . Just) getScores
--         when shouldModify writeScoresFile

isJust          :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False
