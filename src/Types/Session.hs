module Types.Session where

import Types.Classes
import Types.Clocks
import Network.CGI.Cookie
import System.Time (CalendarTime)

data Session = Session
    { currentClass :: Maybe Class
    , currentClock :: Clock }
    deriving (Eq)

makeClassCookie             :: CalendarTime -> Session -> Cookie
makeClassCookie now session = undefined

makeClockCookie             :: CalendarTime -> Session -> Cookie
makeClockCookie now session = undefined
