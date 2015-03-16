import Network.CGI
-- import Network.CGI.Monad
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)
import System.Time (getClockTime, CalendarTime, toCalendarTime)
-- import Control.Monad.Reader
-- import Control.Monad.State
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

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (prettyHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template

page        :: ScoresList -> Html
page scores = (h1 << "Hello World!") +++ rankings (Just $ lookupSariulClass 5 3) scores

cgiMain :: CGI CGIResult
cgiMain = do
    htmlTemplate <- (liftIO . readFile) "html/main.html"
    maybeScores <- liftIO readScoresFile

    let scores = case maybeScores of
            Just s -> s
            _      -> zeroScores

    clockTime <- liftIO getClockTime

    output $ templateInject htmlTemplate (page scores)

main = runCGI . handleErrors $ cgiMain
