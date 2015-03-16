import Network.CGI
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)
import System.Time (getClockTime, CalendarTime, toCalendarTime)

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (prettyHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template

page        :: ScoresList -> Html
page scores = (h1 << "Hello World!") +++ rankings (Just $ lookupSariulClass 5 3) scores

cgiMain                              :: CalendarTime -> String -> ScoresList -> (ScoresList, CGI CGIResult)
cgiMain calendarTime template scores = (scores, output $ templateInject template (page scores))

main :: IO ()
main = do
    htmlTemplate <- readFile "html/main.html"

    -- handle scores file
    scores <- readScoresFile
    let scores' = case scores of
            Just s -> s
            _      -> zeroScores

    clockTime <- getClockTime
    calendarTime <- toCalendarTime clockTime

    let (newScores, cgi) = cgiMain calendarTime htmlTemplate scores'
    if scores' /= newScores
        then writeScoresFile newScores
        else return ()
    runCGI . handleErrors $ cgi
