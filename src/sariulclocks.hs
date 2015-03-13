import Network.CGI
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (prettyHtmlFragment body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template

page        :: ScoresList -> Html
page scores = (h1 << "Hello World!") +++ rankings (Just $ lookupSariulClass 5 3) scores

cgiMain                 :: String -> ScoresList -> (ScoresList, CGI CGIResult)
cgiMain template scores = (scores, output $ templateInject template (page scores))

main :: IO ()
main = do
    htmlTemplate <- readFile "html/main.html"
    scores <- readScoresFile
    let scores' = case scores of
            Just s -> s
            _      -> zeroScores
    let (newScores, cgi) = cgiMain htmlTemplate scores'
    if scores' /= newScores
        then writeScoresFile newScores
        else return ()
    runCGI . handleErrors $ cgi
