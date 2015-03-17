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
import Text.XHtml.Bootstrap

navBar :: Page Html
navBar = return $ paragraph << "navbar here"

makeClockToggle :: Clock -> Html
makeClockToggle = undefined

makeLeftClockButtons                :: Clock -> Html
makeLeftClockButtons CountUpClock   = undefined
makeLeftClockButtons CountDownClock = undefined

makeRightClockButtons :: Html
makeRightClockButtons = primHtml $ "<a id=\"timeWastingClockGo\" class=\"btn btn-primary btn-lg btn-block\">Start <u>t</u>imer</a> <a id=\"timeWastingClockReset\" class=\"btn btn-default btn-lg btn-block\">Re<u>s</u>et timer (end of class)</a>"

clocks :: Page Html
clocks = do
    leftClockType <- liftM (currentClock) getSession
    let leftClockToggle = makeClockToggle leftClockType
    let leftClockClockDiv =
            case leftClockType of
                CountUpClock -> "activity-countup"
                CountDownClock -> "activity-countdown"
    let leftClockClock = thediv ! [strAttr "class" leftClockClockDiv] << noHtml
    let leftClockButtons = makeLeftClockButtons leftClockType
    let leftClock = (<<) clockColumn $
                    (h1 << "Activity time")
                    +++ leftClockToggle +++ br
                    +++ leftClockClock
                    +++ leftClockButtons
    let rightClock = (<<) clockColumn $
                     (h1 << "Time wasting clock") +++ br
                     +++ (thediv ! [strAttr "class" "time-wasting-clock"] << noHtml) +++ br
                     +++ makeRightClockButtons
    return $ thediv ! [strAttr "class" "container"]
        << thediv ! [strAttr "class" "row"]
        << (leftClock +++ rightClock)

clockColumn :: Html -> Html
clockColumn = thediv ! [strAttr "class" "col-md-6"]

makePage :: Page Html
makePage = do
    theNavBar <- navBar
    theClocks <- clocks
    let theDate = paragraph << "date here"
    theRankings <- rankings
    return (theNavBar +++ theClocks +++ theDate +++ theRankings)

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
