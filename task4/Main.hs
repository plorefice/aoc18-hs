{-# LANGUAGE LambdaCase #-}
module Main where

import           Lib
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Dates
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Text.Megaparsec

type GuardId = Int
type Interval = [Int]

data EventType = StartShift Int | Asleep | Awake deriving (Eq, Show)
data Event = Event DateTime EventType deriving (Eq, Show)
data Shift = Shift GuardId DateTime [Interval] deriving (Eq, Show)

main :: IO ()
main = do
  input <- getContents
  let ns = naps . mergeShifts . map parseEvent . sort . lines $ input
  print . uncurry (*) . findMostAsleep $ ns -- Task A
  print . (\(g, (m, _)) -> g * m) . findMostFrequent $ ns -- Task B

findMostAsleep :: Map.Map GuardId Interval -> (GuardId, Int)
findMostAsleep m = (guard, minute)
 where
  guard  = fst . maximumBy (compare `on` (length . snd)) . Map.toList $ m
  minute = head . last . sortOn length . group . sort $ m Map.! guard

findMostFrequent :: Map.Map GuardId Interval -> (GuardId, (Int, Int))
findMostFrequent =
  maximumBy (compare `on` (snd . snd)) . map frequency . Map.toList
 where
  frequency (g, []  ) = (g, (0, 0))
  frequency (g, mins) = (g, (head (f mins), length (f mins)))

  f = maximumBy (compare `on` length) . group . sort

naps :: [Shift] -> Map.Map GuardId Interval
naps = foldl f Map.empty
 where
  f m (Shift id' _ n') = Map.alter
    (\case
      Nothing -> Just (concat n')
      Just n  -> Just (n ++ concat n')
    )
    id'
    m

mergeShifts :: [Event] -> [Shift]
mergeShifts = map makeShift . mergeEvents
 where
  mergeEvents = split (dropInitBlank . keepDelimsL $ whenElt isStartShift)
  isStartShift (Event _ (StartShift _)) = True
  isStartShift _                        = False

makeShift :: [Event] -> Shift
makeShift (Event dt (StartShift id') : evs) = Shift id' (check dt) (naps evs)
 where
  check :: DateTime -> DateTime
  check dt = if hour dt > 0 then addInterval dt (Days 1) else dt

  naps :: [Event] -> [Interval]
  naps [] = []
  naps (Event start Asleep : Event end Awake : evs) =
    [minute start .. minute end - 1] : naps evs

parseEvent :: String -> Event
parseEvent s = case parse event "" s of
  Right e -> e
  Left  e -> error $ show e
 where
  date :: Parser DateTime
  date = do
    sym "["
    y <- int
    sym "-"
    m <- int
    sym "-"
    d  <- int
    hh <- int
    sym ":"
    mm <- int
    sym "]"
    return DateTime { year   = y
                    , month  = m
                    , day    = d
                    , hour   = hh
                    , minute = mm
                    , second = 0
                    }

  eventType :: Parser EventType
  eventType =
    (Asleep <$ sym "falls asleep")
      <|> (Awake <$ sym "wakes up")
      <|> (do
            sym "Guard #"
            id <- int
            sym "begins shift"
            return $ StartShift id
          )

  event :: Parser Event
  event = Event <$> date <*> eventType
