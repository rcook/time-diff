{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Data.Foldable (for_)
import           Data.Time (TimeZone, UTCTime(..), addDays, fromGregorian)
import           Data.Time.Zones (diffForPOSIX, timeZoneForPOSIX)
import           Data.Time.Zones.Internal (utcTimeToInt64)
import           Data.Time.Zones.TH (includeTZFromDB)
import           Data.Time.Zones.Types (TZ)
import           Text.Printf (printf)

newtype Minutes = Minutes Int
instance Show Minutes where
    show (Minutes n) = printf "%d:%02d" (n `div` 60) (n `mod` 60)

tzHaworth :: TZ
tzHaworth = $(includeTZFromDB "Europe/London")

tzBothell :: TZ
tzBothell = $(includeTZFromDB "America/Los_Angeles")

tzOffsetInfo :: TZ -> UTCTime -> (Minutes, TimeZone)
tzOffsetInfo tz utcTime =
    let posixTime = utcTimeToInt64 utcTime
        tzInEffect = timeZoneForPOSIX tz posixTime
        offset = Minutes $ (diffForPOSIX tz posixTime) `div` 60
    in (offset, tzInEffect)

minutesDiff :: Minutes -> Minutes -> Minutes
minutesDiff (Minutes a) (Minutes b) = Minutes (a - b)

main :: IO ()
main = do
    let startDay = fromGregorian 2018 1 1
    for_ (take 365 $ map (flip addDays startDay) [0..]) $ \day -> do
        let time = UTCTime day 0
            (offsetBothell, tzInEffectBothell) = tzOffsetInfo tzBothell time
            (offsetHaworth, tzInEffectHaworth) = tzOffsetInfo tzHaworth time
            diff = offsetBothell `minutesDiff` offsetHaworth
        putStrLn $
            printf
                "%s: %s vs %s %s"
                (show day)
                (show tzInEffectBothell)
                (show tzInEffectHaworth)
                (show diff)
