module Types.Clocks where

data Clock = CountDownClock | CountUpClock
           deriving (Show, Read, Eq)
