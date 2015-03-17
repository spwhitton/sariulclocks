import Network.CGI
-- import Network.CGI.Monad
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)
import System.Time (getClockTime, CalendarTime, toCalendarTime)
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Types.Session
import Types.Clocks
import Control.Monad.Page
import Utils.Classes

navBar :: Page Html
navBar = return $ paragraph << "navbar here"

clocks :: Page Html
clocks = return $ paragraph << "clocks here"

makePage :: Page Html
makePage = do
    theNavBar <- navBar
    theClocks <- clocks
    theRankings <- rankings
    return (theNavBar +++ theClocks +++ theRankings)

-- makePage :: Session -> ScoresList -> (Session, ScoresList, Html)
-- makePage session scores = (session, scores, (h1 << "Hello World!") +++ rankings (Just $ lookupSariulClass 5 3) scores)

cgiMain :: CGI CGIResult
cgiMain = do
    -- preparatory IO: templating, scores file, time (for cookies)

    htmlTemplate <- (liftIO . readFile) "html/main.html"
    maybeScores <- liftIO readScoresFile
    let scores = case maybeScores of
            Just s -> s
            _      -> zeroScores

    clockTime <- liftIO getClockTime

    currentClock <- liftM (fromMaybe 0) $ readCookie "clock_cookie"
    currentClass <- liftM (parseClassCookie) $ getCookie "class_cookie"
    let session = Session { currentClass = currentClass
                          , currentClock =
                              case currentClock of
                                  0 -> CountDownClock
                                  1 -> CountUpClock}

    -- now do our CGI work

    let (newScores, newSession, html) = runPage makePage scores session

    output $ templateInject htmlTemplate html

main = runCGI . handleErrors $ cgiMain

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (prettyHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template
