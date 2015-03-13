import Network.CGI
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores
import Data.Classes

page        :: ScoresList -> Html
page scores = body << ((h1 << "Hello World!") +++ rankings Nothing scores)

cgiMain        :: ScoresList -> (ScoresList, CGI CGIResult)
cgiMain scores = (scores, output $ prettyHtml (page scores))

main :: IO ()
main = do
    scores <- readScoresFile
    let scores' = case scores of
            Just s -> s
            _      -> zeroScores
    let (newScores, cgi) = cgiMain scores'
    if scores' /= newScores
        then writeScoresFile newScores
        else return ()
    runCGI . handleErrors $ cgi
