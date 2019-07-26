{-# LANGUAGE QuasiQuotes #-}

module Ch24.LogFile where

import Control.Monad
import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Data.Time
import Data.Time.LocalTime
import Data.Foldable
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (sort)
import Safe (headMay, lastMay)

data Activity =
    Activity String
    deriving (Eq)

instance Show Activity where
    show (Activity s) = s

data DayLog =
    DayLog Day (Map UTCTime Activity)
    deriving (Eq)

instance Show DayLog where
    show (DayLog day activities) =
        let dayString = "# " ++ show day
            dayLog = showAct <$> (M.toList activities)
            showAct (dateTime, act) =
                showTime time ++ " " ++ show act ++ "\n"
                    where (UTCTime _ time) = dateTime
         in dayString ++ "\n" ++ fold dayLog

showTime :: DiffTime -> String
showTime diff = pad2 (show h) ++ ":" ++ pad2 (show m)
    where time = timeToTimeOfDay diff
          h = todHour time
          m = todMin time

pad2 :: String -> String
pad2 s =
    case length s of
      0 -> "00"
      1 -> "0" ++ s
      _ -> s

totalTimePerDay :: DayLog -> Maybe NominalDiffTime
totalTimePerDay (DayLog _ activities) =
    let times = sort . fmap fst . M.toList $ activities
        start = headMay times
        end   = lastMay times
     in liftA2 diffUTCTime end start
    
averageActivityTimePerDay :: DayLog -> Maybe NominalDiffTime
averageActivityTimePerDay daylog@(DayLog _ activities) =
    let timeDiff = totalTimePerDay daylog
        numberOfActivities = fromIntegral . M.size $ activities
     in fmap (/numberOfActivities) timeDiff

averageActivityTime :: [DayLog] -> Maybe NominalDiffTime
averageActivityTime daylogs =
    fmap (calcAvg . sum) $ traverse averageActivityTimePerDay daylogs
        where calcAvg total = total / (fromIntegral $ length daylogs)

parseLogFile :: Parser [DayLog]
parseLogFile = some parseDayLog

parseDayLog :: Parser DayLog
parseDayLog = do
    skipNewLines
    skipMany comment
    date <- parseDate
    activities <- some (activity date)
    skipNewLines
    return $ DayLog date (M.fromList activities)

activity :: Day -> Parser (UTCTime, Activity)
activity day = do
    h <- integer
    _ <- char ':'
    m <- integer
    act <- activityName
    skipNewLines
    let time = timeOfDay h m
    return $ (UTCTime day time, Activity act)

activityName :: Parser String
activityName =
    try (manyTill exceptNewLine comment) <|> many exceptNewLine

parseDate :: Parser Day
parseDate = do
    _ <- string "# "
    y <- integer
    _ <- char '-'
    m <- integer
    _ <- char '-'
    d <- integer
    skipMany comment
    return $ fromGregorian y (fromInteger m) (fromInteger d)

comment :: Parser ()
comment = do
    skipMany (oneOf " ")
    string "--"
    skipMany exceptNewLine
    skipNewLines

exceptNewLine :: Parser Char
exceptNewLine = noneOf "\n"

skipNewLines :: Parser ()
skipNewLines = skipMany (oneOf "\n")

timeOfDay :: Integer -> Integer -> DiffTime
timeOfDay h m = 3600 * (fromInteger h) +
                60 * (fromInteger m)

logFileEx :: String
logFileEx = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

spec :: IO ()
spec = do
    print $ parseString comment mempty "   -- hello\n"
    print $ parseString comment mempty "-- hello\n --hello\n"
    print $ parseString parseDate mempty "# 2019-07-21\n"
    let logFile = parseString parseLogFile mempty logFileEx
    print $ logFile
    print $ (fmap . fmap) totalTimePerDay logFile
    print $ (fmap . fmap) averageActivityTimePerDay logFile
    print $ fmap averageActivityTime logFile
