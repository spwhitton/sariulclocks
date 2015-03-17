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
navBar = return $
         thediv # "navbar navbar-inverse navbar-fixed-top" ! [strAttr "role" "navigation"]
         << thediv # "container"
         << primHtml "<div class=\"navbar-header\"> <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\"> <span class=\"sr-only\">Toggle navigation</span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> </button> <a class=\"navbar-brand\" href=\"#\">Mr Whitton's timers</a> </div>"

makeClockToggle   :: Clock -> Html
makeClockToggle _ = bsButton "leftClockToggle" "btn btn-info" "Count up/down toggle"

makeLeftClockButtons                :: Clock -> Html
makeLeftClockButtons CountUpClock   = stringToHtml "start, stop, reset?"
makeLeftClockButtons CountDownClock = (paragraph # "text-center" << timeButtons)
                                      +++ (paragraph # "text-center" << controlButtons)
                                      +++ (paragraph # "text-center"
                                           << "Hotkeys: press the number key for the number of minutes you want to countdown.")
  where
    timeButtons                     = foldr (+++) noHtml $
                                      [ bsButton "activityClock30s"  "btn btn-primary btn-lg" ("3" +++ (underline << "0") +++ "s")
                                      , bsButton "activityClock60s"  "btn btn-primary btn-lg" "1m"
                                      , bsButton "activityClock90s"  "btn btn-primary btn-lg" ((underline << "9") +++ "0s")
                                      , bsButton "activityClock120s" "btn btn-primary btn-lg" "2m"
                                      , bsButton "activityClock180s" "btn btn-primary btn-lg" "3m"
                                      , bsButton "activityClock240s" "btn btn-primary btn-lg" "4m"
                                      , bsButton "activityClock300s" "btn btn-primary btn-lg" "5m" ]
    controlButtons                  = foldr (+++) noHtml $
                                      [ bsButton "activityClockCustom" "btn btn-default btn-lg" ((underline << "C") +++ "ustom")
                                      , bsButton "activityClockReset" "btn btn-default btn-lg" ((underline << "R") +++ "eset")]

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
                    (h1 << ("Activity time" +++ " " +++ leftClockToggle))
                    +++ br
                    +++ leftClockClock
                    +++ leftClockButtons
    let rightClock = (<<) clockColumn $
                     (h1 << "Time wasting clock") +++ br
                     +++ (thediv ! [strAttr "class" "time-wasting-clock"] << noHtml) +++ br
                     +++ makeRightClockButtons
    return $ thediv # "container"
        << thediv # "row"
        << (leftClock +++ rightClock)

clockColumn :: Html -> Html
clockColumn = thediv ! [strAttr "class" "col-md-6"]

theDate :: Html
theDate = thediv # "container" << thediv # "row"
          << thediv # "col-md-12"
          << (hr
              +++ (h1 ! [ strAttr "id" "date"
                        , strAttr "class" "text-center"]
                   << noHtml) +++ hr)

makePage :: Page Html
makePage = do
    theNavBar <- navBar
    theClocks <- clocks
    theRankings <- rankings
    return (theNavBar +++ theClocks +++ theDate +++ theRankings)

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
templateInject template body = templateBefore ++ (showHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template
