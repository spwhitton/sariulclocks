import Data.Text (strip, unpack, pack)
import Network.CGI
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
import Control.Monad.SariulClocks
import Utils.Classes
import Text.XHtml.Bootstrap
import Control.Exception (onException)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Utils.Xhtml
import Network.URI
import System.FilePath (takeDirectory)

--- functions making HTML

navBar :: SariulClocksCGI Html
navBar = do
    currentClass <- liftM (currentClass) getSession
    return $
         thediv # "navbar navbar-inverse navbar-fixed-top" ! [strAttr "role" "navigation"]
         << thediv # "container"
         << (primHtml "<div class=\"navbar-header\"> <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\"> <span class=\"sr-only\">Toggle navigation</span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> <span class=\"icon-bar\"></span> </button> <a class=\"navbar-brand\" href=\"#\">Mr Whitton's timers</a> </div>"
         +++ (thediv # "navbar-collapse collapse" << form #= "end_of_class_form" # "navbar-form navbar-right" ! [strAttr "role" "form", strAttr "name" "end_of_class_form", strAttr "method" "POST"]
             << (   lessonButtons currentClass
                +++ bsButton "date-toggle" "btn btn-default" ("Toggle " +++ uC 'd' +++ "ate style")
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
lessonButtons Nothing  = bsButton "start-lesson" "btn btn-info" ("Start " +++ uC 'l' +++ "esson")
                         -- +++ bsButton "end-of-week" "btn btn-default" "End of week"
lessonButtons (Just _) = primHtml "<div class=\"form-group\"> <input id=\"class_points\" name=\"class_points\" type=\"text\" placeholder=\"Points scored\" class=\"form-control\"> </div> <div class=\"form-group\"> <input type=\"password\" id=\"teachers_password\" name=\"teachers_password\" placeholder=\"Teacher's password\" class=\"form-control\"> </div> "
                          +++ input #= "class_time_wasted" ! [strAttr "name" "class_time_wasted", strAttr "type" "hidden", strAttr "value" ""]
                          +++ bsButton "end-lesson" "btn btn-success" "End lesson"
                          +++ bsButton "lucky-number" "btn btn-danger" (uC 'L' +++ "ucky number")

makeClockToggle   :: Clock -> Html
makeClockToggle _ = bsButton "leftClockToggle" "btn btn-info" "Count up/down toggle"

makeLeftClockButtons                :: Clock -> Html
makeLeftClockButtons CountUpClock   = paragraph # "text-center" << controlButtons
  where
    controlButtons = (+++) br $ foldr (+++) noHtml $
                      [ bsButton "activityClockUpGo" "btn btn-primary btn-lg btn-block" ("Start stopwatch (" +++ uC 'a' +++ ")")
                      , bsButton "activityClockUpReset" "btn btn-default btn-lg btn-block" ("Reset stopwatch (" +++ uC 'z' +++ ")")]
makeLeftClockButtons CountDownClock = br +++ (paragraph # "text-center" << timeButtons)
                                      +++ (paragraph # "text-center" << controlButtons)
                                      +++ (paragraph # "text-center"
                                           << "Hotkeys: press the number key for the number of minutes you want to countdown.")
  where
    timeButtons                     = foldr (+++) noHtml $
                                      [ bsButton "activityClock30s"  "btn btn-primary btn-lg" ("3" +++ (uC '0') +++ "s")
                                      , bsButton "activityClock60s"  "btn btn-primary btn-lg" "1m"
                                      , bsButton "activityClock90s"  "btn btn-primary btn-lg" ((uC '9') +++ "0s")
                                      , bsButton "activityClock120s" "btn btn-primary btn-lg" "2m"
                                      , bsButton "activityClock180s" "btn btn-primary btn-lg" "3m"
                                      , bsButton "activityClock240s" "btn btn-primary btn-lg" "4m"
                                      , bsButton "activityClock300s" "btn btn-primary btn-lg" "5m" ]
    controlButtons                  = foldr (+++) noHtml $
                                      [ bsButton "activityClockCustom" "btn btn-default btn-lg" (uC 'C' +++ "ustom")
                                      , bsButton "activityClockReset" "btn btn-default btn-lg" (uC 'R' +++ "eset")]

makeRightClockButtons :: Html
makeRightClockButtons = primHtml $ "<a id=\"timeWastingClockGo\" class=\"btn btn-primary btn-lg btn-block\">Start <u>t</u>imer</a> <a id=\"timeWastingClockReset\" class=\"btn btn-default btn-lg btn-block\">Re<u>s</u>et timer </a>"

clocks :: SariulClocksCGI Html
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

--- main functions

makePage :: SariulClocksCGI Html
makePage = do
    theNavBar <- navBar
    theClocks <- clocks
    theRankings <- rankings
    return (theNavBar +++ theClocks +++ theDate +++ theRankings)

cgiMain :: SariulClocksCGI CGIResult
cgiMain = do
    -- preparatory IO: templating, scores file, time (for cookies)

    password <- (liftIO . readFile) "password"
    userPassword <- liftM (fromMaybe "") $ getInput "teachers_password"
    let passwordPasses = (unpack . strip . pack) password == userPassword

    htmlTemplate <- (liftIO . readFile) "html/main.html"
    clockTime <- liftIO getClockTime
    scores <- readScoresFile

    points <- liftM (readInt . fromMaybe "0") $ getInput "class_points"
    timeWasted <- liftM (readInt . fromMaybe "0") $ getInput "class_time_wasted"
    cookieClock <- liftM (fromMaybe 0) $ readCookie "clock_cookie"
    cookieClass <- liftM (parseClassCookie) $ getCookie "class_cookie"

    putSession Session { currentClass = if passwordPasses then Nothing else cookieClass
                       , currentClock =
                              case cookieClock of
                                  0 -> CountDownClock
                                  1 -> CountUpClock }
    when passwordPasses $ modifyScores (updateScore (fromJust cookieClass) points timeWasted)

    -- It would be good to use POST,REDIRECT,GET
    -- <http://stackoverflow.com/a/570069> here.  However, you can't
    -- reliably set cookies during a 302 redirect: a lot of browsers
    -- cough on it.  So we'd have to change lots of things about our
    -- application to make this work.

    selfURL <- liftM uriPath requestURI
    page <- makePage

    setCookie =<< liftM (makeClassCookie clockTime (takeDirectory selfURL)) getSession
    setCookie =<< liftM (makeClockCookie clockTime (takeDirectory selfURL)) getSession
    setCookie =<< liftM (makeSsCookie clockTime    (takeDirectory selfURL)) getSession
    shouldModify <- liftM (((/=) scores) . Just) getScores
    when shouldModify writeScoresFile

    output $ templateInject htmlTemplate page

main = runSariulClocksCGI cgiMain

--- utility functions

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
