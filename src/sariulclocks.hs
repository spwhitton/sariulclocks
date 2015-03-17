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
-- import Control.Monad.Reader
import Control.Monad.State
-- import System.IO (stdin, stdout)

-- Monad stack: scores list state -> CGI -> IO.  This is mostly black
-- magic to me at this point, from
-- <https://wiki.haskell.org/Web/Literature/Practical_web_programming_in_Haskell#The_CGI_Monad>.

-- newtype AppT m a = App (StateT ScoresList (CGIT m) a)
--                  deriving (Monad, MonadIO, MonadState ScoresList)
-- type App a       = AppT IO a

-- instance MonadCGI (AppT IO) where
--     cgiAddHeader n v = App . lift $ cgiAddHeader n v
--     cgiGet x         = App . lift $ cgiGet x

-- -- runApp         :: App CGIResult -> IO ()
-- -- runApp (App a) = runCGIT (evalStateT a zeroScores)

-- End the black magic.

-- Page monad stack for generating the page

type Page = StateT Session (State ScoresList)

runPage :: Page a -> ScoresList -> Session -> (ScoresList, Session, a)
runPage k scores session = (scores', session', a)
  where
    ((a, session'), scores') = runState (runStateT k session) scores

putSession :: Session -> Page ()
putSession = put

getSession :: Page Session
getSession = get

getScores :: Page ScoresList
getScores = lift get

putScores :: ScoresList -> Page ()
putScores = lift . put

makePage :: Page Html
makePage = return (h1 << "Hello World")

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
