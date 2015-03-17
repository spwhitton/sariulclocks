module Control.Monad.Page where

import Control.Monad.State
import Control.Monad.Trans (lift)
import Types.Session
import Types.Scores
import Types.Classes

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
