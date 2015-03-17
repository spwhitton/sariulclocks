module Types.Session where

import Types.Classes
import Types.Clocks
import Network.CGI.Cookie
import System.Time
import Data.List.Split (splitOn)
import Data.Classes
import Data.Maybe (fromJust)

data Session = Session
    { currentClass :: Maybe Class
    , currentClock :: Clock }
    deriving (Eq)

-- If we were using a proper monad stack, this should be of type App
-- Cookie where it uses the reader monad to get the CalendarTime and
-- maybe the session from the state monad

makeClassCookie             :: ClockTime -> Session -> Cookie
makeClassCookie now session =
    Cookie { cookieName     = "class_cookie"
           , cookieValue    =
               case currentClass session of
                   Just c  -> show c
                   Nothing -> "Nothing"
           , cookieExpires  = Just $ endOfSchoolDay now
           , cookieDomain   = Nothing
           , cookiePath     = Nothing
           , cookieSecure   = False}

makeClockCookie             :: ClockTime -> Session -> Cookie
makeClockCookie now session =
    Cookie { cookieName     = "clock_cookie"
           , cookieValue    =
               case currentClock session of
                   CountDownClock -> "0"
                   CountUpClock   -> "1"
           , cookieExpires  = Just $ endOfSchoolDay now
           , cookieDomain   = Nothing
           , cookiePath     = Nothing
           , cookieSecure   = False}

endOfSchoolDay       :: ClockTime -> CalendarTime
endOfSchoolDay now   = (toUTCTime . addToClockTime noTimeDiff { tdHour = hoursTilEndOfDay }) now
  where
    koreanTime       = ((toUTCTime . addToClockTime noTimeDiff { tdHour = 9 }) now)
                       { ctTZ = 9 * 60 * 60
                       , ctTZName = "KST"}
    hoursTilEndOfDay = if   hoursTilEndOfDay' > 0
                       then hoursTilEndOfDay'
                       else 1
    hoursTilEndOfDay' = 18 - ctHour koreanTime

-- could use Maybe monad in the below!

parseClassCookie   :: Maybe String -> Maybe Class
parseClassCookie s =
    case s of
        Just s  -> parseClassCookie' s
        Nothing -> Nothing

parseClassCookie' :: String -> Maybe Class
parseClassCookie' s =
    case splitOn "-" s of
        g:c:[] -> Just $ lookupSariulClass (read g) (read c)
        _      -> Nothing
