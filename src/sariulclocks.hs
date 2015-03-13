import Network.CGI
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes
import Data.List.Split (splitOn)

templateInject               :: String -> Html -> String
templateInject template body = templateBefore ++ (prettyHtml body) ++ templateAfter
  where
    (templateBefore:templateAfter:_) = splitOn "BODY_HERE" template

page        :: ScoresList -> Html
page scores = body << ((h1 << "Hello World!") +++ rankings Nothing scores)

cgiMain                 :: String -> ScoresList -> (ScoresList, CGI CGIResult)
cgiMain template scores = (scores, output $ templateInject template (page scores))

main :: IO ()
main = do
    htmlTemplate <- readFile "assets/html/main.html"
    scores <- readScoresFile
    let scores' = case scores of
            Just s -> s
            _      -> zeroScores
    let (newScores, cgi) = cgiMain htmlTemplate scores'
    if scores' /= newScores
        then writeScoresFile newScores
        else return ()
    runCGI . handleErrors $ cgi
