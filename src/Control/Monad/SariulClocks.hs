{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.SariulClocks ( SariulScoresMonad
                                  , SariulClocksCGI
                                  , SariulClocksIO
                                  , runSariulClocksCGI
                                  , runSariulClocksIO
                                  , putSession
                                  , getSession
                                  , putScores
                                  , getScores
                                  , modifyScores) where

import           Control.Monad       (liftM)
import           Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import           Control.Monad.Trans (MonadIO, lift)
import           Data.Classes
import           Network.CGI         (CGIResult, handleErrors, runCGI)
import           Network.CGI.Monad   (CGIT, MonadCGI, cgiAddHeader, cgiGet)
import           Types.Classes
import           Types.Scores
import           Types.Session

class ( Monad a
      , MonadIO a) => SariulScoresMonad a where
    putScores       :: ScoresList -> a ()
    getScores       :: a ScoresList
    modifyScores    :: (ScoresList -> ScoresList) -> a ()
    modifyScores f  = do
        scores <- getScores
        let scores' = f scores
        putScores scores
        return ()

newtype SariulClocksCGI a =
    SCC { getSCC :: StateT (Session, ScoresList) (CGIT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Session, ScoresList))

newtype SariulClocksIO a =
    SCI { getSCI :: StateT ScoresList IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ScoresList)

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
