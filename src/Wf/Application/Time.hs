module Wf.Application.Time
( Time
, formatTime
, diffTime
, addSeconds
, getCurrentTime
, mjd
) where

import qualified Data.Time.Clock as T (UTCTime(..), diffUTCTime, addUTCTime, getCurrentTime)
import qualified Data.Time.Calendar as T (Day(..))
import qualified Data.Time.Format as T (formatTime)
import System.Locale (defaultTimeLocale)

type Time = T.UTCTime

formatTime :: String -> Time -> String
formatTime = T.formatTime defaultTimeLocale

diffTime :: Time -> Time -> Integer
diffTime a b = floor $ T.diffUTCTime a b

addSeconds :: Time -> Integer -> Time
addSeconds t s = T.addUTCTime (fromInteger s) t

getCurrentTime :: IO Time
getCurrentTime = T.getCurrentTime

mjd :: Time
mjd = T.UTCTime (T.ModifiedJulianDay 0) 0
