{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.SariulClocks ( SariulScoresMonad
                                  , SariulClocksCGI
                                  , SariulClocksIO
                                  , putSession
                                  , getSession
                                  , putScores
                                  , getScores) where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.State (StateT, MonadState, get, put, evalStateT)
import Types.Session
import Types.Scores
import Types.Classes
import Data.Classes
import Network.CGI.Monad (CGIT, MonadCGI, cgiAddHeader, cgiGet)
import Network.CGI (runCGI, handleErrors, CGIResult)

class ( Monad a
      , MonadIO a) => SariulScoresMonad a where
    putScores       :: ScoresList -> a ()
    getScores       :: a ScoresList

newtype SariulClocksCGI a =
    SCC { getSCC :: StateT (Session, ScoresList) (CGIT IO) a }
    deriving (Monad, MonadIO, MonadState (Session, ScoresList))

newtype SariulClocksIO a =
    SCI { getSCI :: StateT ScoresList IO a }
    deriving (Monad, MonadIO, MonadState ScoresList)

instance MonadCGI SariulClocksCGI where
    cgiAddHeader n v = SCC . lift $ cgiAddHeader n v
    cgiGet x = SCC . lift $ cgiGet x

instance SariulScoresMonad SariulClocksCGI where
    getScores = liftM snd get
    putScores s = do
        (x, _) <- get
        put (x, s)

instance SariulScoresMonad SariulClocksIO where
    getScores = get
    putScores = put

putSession   :: Session -> SariulClocksCGI ()
putSession s = do
        (_, y) <- get
        put (s, y)

getSession :: SariulClocksCGI Session
getSession = liftM fst get

runSariulClocksCGI   :: SariulClocksCGI CGIResult -> IO ()
runSariulClocksCGI k =
        runCGI $ handleErrors $
        evalStateT (getSCC k) (freshSession, zeroScores)

runSariulClocksIO  :: SariulClocksIO () -> IO ()
runSariulClocksIO k = evalStateT (getSCI k) zeroScores
