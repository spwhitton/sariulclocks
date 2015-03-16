module Types.Session where

import Types.Classes
import Types.Clocks
import Network.CGI.Cookie
import System.Time

data Session = Session
    { currentClass :: Maybe Class
    , currentClock :: Clock }
    deriving (Eq)

-- If we were using a proper monad stack, this should be of type App
-- Cookie where it uses the reader monad to get the CalendarTime and
-- maybe the session from the state monad

makeClassCookie             :: ClockTime -> Session -> Cookie
makeClassCookie now session = undefined

makeClockCookie             :: ClockTime -> Session -> Cookie
makeClockCookie now session = undefined

endOfSchoolDay       :: ClockTime -> CalendarTime
endOfSchoolDay now   = (toUTCTime . addToClockTime noTimeDiff { tdHour = hoursTilEndOfDay }) now
  where
    koreanTime       = ((toUTCTime . addToClockTime noTimeDiff { tdHour = 9 }) now)
                       { ctTZ = 9 * 60 * 60
                       , ctTZName = "KST"}
    hoursTilEndOfDay = 18 - ctHour koreanTime
