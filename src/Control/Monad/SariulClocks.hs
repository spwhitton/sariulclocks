{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.SariulClocks ( SariulClocks
                                  , putSession
                                  , getSession
                                  , putScores
                                  , getScores) where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.State (StateT, MonadState, get, put)
import Types.Session
import Types.Scores
import Types.Classes
import Network.CGI.Monad (CGIT, MonadCGI, cgiAddHeader, cgiGet)

newtype SariulClocks a =
    SC { runSariulClocks :: StateT (Session, ScoresList) (CGIT IO) a }
    deriving (Monad, MonadIO, MonadState (Session, ScoresList))

instance MonadCGI SariulClocks where
    cgiAddHeader n v = SC . lift $ cgiAddHeader n v
    cgiGet x = SC . lift $ cgiGet x

putSession   :: Session -> SariulClocks ()
putSession s = do
    (_, y) <- get
    put (s, y)

getSession :: SariulClocks Session
getSession = liftM fst get

getScores :: SariulClocks ScoresList
getScores = liftM snd get

putScores   :: ScoresList -> SariulClocks ()
putScores s = do
    (x, _) <- get
    put (x, s)
