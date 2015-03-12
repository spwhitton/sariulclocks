import Network.CGI
import Text.XHtml
import Utils.ScoresFile (readScoresFile,writeScoresFile)
import Types.Classes
import Types.Scores

page :: Html
page = body << h1 << "Hello World!"

cgiMain        :: [(Class,Score)] -> ([(Class,Score)],CGI CGIResult)
cgiMain scores = (scores, output $ renderHtml page)

main :: IO ()
main = do
    scores <- readScoresFile
    (newScores,cgi) <- cgiMain scores
    writeScoresFile newScores
    runCGI . handleErrors $ cgi
