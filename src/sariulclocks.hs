import Data.Text (strip, unpack, pack)
import Network.CGI
-- import Network.CGI.Monad
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)
import System.Time (getClockTime, CalendarTime, toCalendarTime)
import Control.Monad (liftM, when)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Types.Session
import Types.Clocks
import Control.Monad.Page
import Utils.Classes
import Text.XHtml.Bootstrap
import Control.Exception (onException)
import Data.Char (isDigit)
import Data.Maybe (fromJust)

navBar :: Page Html
navBar = do
    currentClass <- liftM (currentClass) getSession
    return $
         thediv # "navbar navbar-inverse navbar-fixed-top" ! [strAttr "role" "navigation"]
         << thediv # "container"
         << (primHtml "<div class=\"navbar-header\"> <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\"> <span class=\"sr-only\">Toggle navigation</span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> </button> <a class=\"navbar-brand\" href=\"#\">Mr Whitton's timers</a> </div>"
         +++ (thediv # "navbar-collapse collapse" << form #= "end_of_class_form" # "navbar-form navbar-right" ! [strAttr "role" "form", strAttr "name" "end_of_class_form", strAttr "method" "POST"]
             << (   lessonButtons currentClass
                +++ bsButton "date-toggle" "btn btn-default" ("Toggle " +++ (underline << "d") +++ "ate style")
                +++ soundsButton)))
  where
    soundsButton = thediv # "btn-group"
                   << (anchor # "btn btn-default dropdown-toggle"
                             ! [ strAttr "data-toggle" "dropdown"
                               , strAttr "aria-expanded" "false"
                               , strAttr "role" "button"
                               , strAttr "href" "#"]
                             << ("Play sound " +++ thespan # "caret" << noHtml)
                   +++ ulist # "dropdown-menu" ! [strAttr "role" "menu"]
                   << ((li << anchor #= "klaxon" ! [strAttr "href" "#"] << "Klaxon")
                       +++ (li << anchor #= "bell" ! [strAttr "href" "#"] << "Bell")
                       +++ (li # "divider" << noHtml)
                       +++ (li # "dropdown-header" << "Sean")
                       +++ (li << anchor #= "one-two-three" ! [strAttr "href" "#"] << "One, two, three")
                      +++ (li << anchor #= "too-noisy" ! [strAttr "href" "#"] << "Too noisy")
                      +++ (li << anchor #= "sit-down-quickly" ! [strAttr "href" "#"] << "Sit down quickly")
                       +++ (li # "divider" << noHtml)
                       +++ (li # "dropdown-header" << "은아")
                      +++ (li << anchor #= "why-so-noisy" ! [strAttr "href" "#"] << "Why are you so noisy?")))

lessonButtons          :: Maybe Class -> Html
lessonButtons Nothing  = bsButton "start-lesson" "btn btn-info" "Start lesson"
                         -- +++ bsButton "end-of-week" "btn btn-default" "End of week"
lessonButtons (Just _) = primHtml "<div class=\"form-group\"> <input id=\"class_points\" name=\"class_points\" type=\"text\" placeholder=\"Points scored\" class=\"form-control\"> </div> <div class=\"form-group\"> <input type=\"password\" id=\"teachers_password\" name=\"teachers_password\" placeholder=\"Teacher's password\" class=\"form-control\"> </div> "
                          +++ input #= "class_time_wasted" ! [strAttr "name" "class_time_wasted", strAttr "type" "hidden", strAttr "value" ""]
                          +++ bsButton "end-lesson" "btn btn-success" "End lesson"
                          +++ bsButton "lucky-number" "btn btn-danger" ((underline << "L") +++ "ucky number")

makeClockToggle   :: Clock -> Html
makeClockToggle _ = bsButton "leftClockToggle" "btn btn-info" "Count up/down toggle"

makeLeftClockButtons                :: Clock -> Html
makeLeftClockButtons CountUpClock   = paragraph # "text-center" << controlButtons
  where
    controlButtons = (+++) br $ foldr (+++) noHtml $
                      [ bsButton "activityClockUpGo" "btn btn-primary btn-lg btn-block" ("Start stopwatch (" +++ (underline << "a") +++ ")")
                      , bsButton "activityClockUpReset" "btn btn-default btn-lg btn-block" ("Reset stopwatch (" +++ (underline << "z") +++ ")")]
makeLeftClockButtons CountDownClock = br +++ (paragraph # "text-center" << timeButtons)
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
makeRightClockButtons = primHtml $ "<a id=\"timeWastingClockGo\" class=\"btn btn-primary btn-lg btn-block\">Start <u>t</u>imer</a> <a id=\"timeWastingClockReset\" class=\"btn btn-default btn-lg btn-block\">Re<u>s</u>et timer </a>"

clocks :: Page Html
clocks = do
    leftClockType <- liftM (currentClock) getSession
    let leftClockToggle = makeClockToggle leftClockType
    let leftClockClockDiv =
            case leftClockType of
                CountUpClock -> "activity-countup"
                CountDownClock -> "activity-countdown"
    let leftClockClock = thediv ! [strAttr "id" leftClockClockDiv] << noHtml
    let leftClockButtons = makeLeftClockButtons leftClockType
    let leftClock = (<<) clockColumn $
                    (h1 << ("Activity time" +++ " " +++ leftClockToggle))
                    +++ br
                    +++ leftClockClock
                    +++ leftClockButtons
    currentClass <- liftM (currentClass) getSession
    let rightClock = (<<) clockColumn $
                     case currentClass of
                         Just _ -> (h1 << "Time wasting clock") +++ br
                                   +++ (thediv ! [strAttr "id" "time-wasting-clock"] << noHtml) +++ br
                                   +++ makeRightClockButtons
                         Nothing -> noHtml
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

-- forms :: Page Html
-- forms = do
--     let html = form #= "end_of_class_form" ! [strAttr "method" "POST"] << (input #= "class_points" ! [strAttr "name" "class_points", strAttr "type" "hidden", strAttr "value" ""] +++ input #= "class_time_wasted" ! [strAttr "name" "class_time_wasted", strAttr "type" "hidden", strAttr "value" ""])
--     return html

makePage :: Page Html
makePage = do
    theNavBar <- navBar
    theClocks <- clocks
    theRankings <- rankings
    return (theNavBar +++ theClocks +++ theDate +++ theRankings)

cgiMain :: CGI CGIResult
cgiMain = do
    -- preparatory IO: templating, scores file, time (for cookies)

    password <- (liftIO . readFile) "password"
    userPassword <- liftM (fromMaybe "") $ getInput "teachers_password"
    htmlTemplate <- (liftIO . readFile) "html/main.html"
    maybeScores <- liftIO readScoresFile
    let scores = case maybeScores of
            Just s -> s
            _      -> zeroScores

    clockTime <- liftIO getClockTime

    points <- liftM (fromMaybe "0") $ getInput "class_points"
    timeWasted <- liftM (fromMaybe "0") $ getInput "class_time_wasted"
    let points' = readInt points
    let timeWasted' = readInt timeWasted

    cookieClock <- liftM (fromMaybe 0) $ readCookie "clock_cookie"
    cookieClass <- liftM (parseClassCookie) $ getCookie "class_cookie"

    let session = Session { currentClass = if points' /= 0 && userPassword == (unpack . strip . pack) password then Nothing else cookieClass
                          , currentClock =
                              case cookieClock of
                                  0 -> CountDownClock
                                  1 -> CountUpClock}

    -- now do our CGI work

    -- TODO: use POST,REDIRECT,GET https://stackoverflow.com/questions/570015/how-do-i-reload-a-page-without-a-postdata-warning-in-javascript/570069#570069

    -- TODO: restore time wasting clock if password was wrong

    let scores' =
            if   points' /= 0 && userPassword == (unpack . strip . pack) password
            then updateScore scores (fromJust cookieClass) points' timeWasted'
            else scores

    let (newScores, newSession, html) = runPage makePage scores' session

    setCookie $ makeClassCookie clockTime newSession
    setCookie $ makeClockCookie clockTime newSession
    setCookie $ makeSsCookie clockTime newSession

    -- let html' = html +++ show points' +++ show timeWasted'

    when (newScores /= scores) $ liftIO $ writeScoresFile newScores

    output $ templateInject htmlTemplate html

main = runCGI . handleErrors $ cgiMain

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (renderHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template

readInt        :: String -> Int
readInt string = if null string'
                 then 0
                 else read string'
  where
    string' = filter isDigit string
