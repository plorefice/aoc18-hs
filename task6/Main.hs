module Main where

import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Function

type LocationId = Int
type Distance = Int

data Location = Location LocationId (Int, Int) deriving Show
data Rect = Rect (Int, Int) (Int, Int) deriving Show

main :: IO ()
main = do
  input <- getContents
  let coords =
        map (\(i, [a, b]) -> Location i (read a, read b))
          . zip [0 :: LocationId ..]
          . map (splitOn ", ")
          . lines
          $ input
      b0 = boundingRect coords

  -- Task A
  let b1 = slightlyBigger b0
      g0 = furthestLocations coords b0
      g1 = furthestLocations coords b1
      g  = filter (\((_, n), (_, n')) -> n == n')
        $ zip (Map.toAscList g0) (Map.toAscList g1)
  print . snd . maximumBy (compare `on` snd) . map fst $ g

  -- Task B
  let pts = grid b0
  print
    . length
    . filter ((< 10000) . snd)
    . zip pts
    . map (\pt -> sum . map (manhattan pt) $ coords)
    $ pts

boundingRect :: [Location] -> Rect
boundingRect = foldl f (Rect (1000000, 1000000) (0, 0))
 where
  f (Rect (x, y) (x', y')) (Location _ (nx, ny)) =
    Rect (min x nx, min y ny) (max x' nx, max y' ny)

slightlyBigger :: Rect -> Rect
slightlyBigger (Rect (x, y) (x', y')) = Rect (x - 1, y - 1) (x' + 1, y' + 1)

furthestLocations :: [Location] -> Rect -> Map LocationId Int
furthestLocations pts r = foldl update' Map.empty $ grid r
 where
  update' m coord = Map.alter f (closestTo coord) m
  f old = case old of
    Nothing -> Just 1
    Just n  -> Just (n + 1)
  closestTo coord =
    takeClosestOrNone
      . groupBy ((==) `on` snd)
      . sortBy (compare `on` snd)
      . zip pts
      . map (manhattan coord)
      $ pts
  takeClosestOrNone (ls : _) =
    if length ls > 1 then -1 else (\(Location id' _, _) -> id') . head $ ls

grid :: Rect -> [(Int, Int)]
grid (Rect (x, y) (x', y')) = [ (x, y) | x <- [x .. x'], y <- [y .. y'] ]

manhattan :: (Int, Int) -> Location -> Distance
manhattan (x, y) (Location _ (x', y')) = abs (x - x') + abs (y - y')
