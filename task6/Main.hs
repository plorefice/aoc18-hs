module Main where

import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Function

data Location = Location Int (Int, Int) deriving Show
data Rect = Rect (Int, Int) (Int, Int) deriving Show
type Grid = Map Int Int

main :: IO ()
main = do
  input <- getContents
  let coords =
        map (\(i, [a, b]) -> Location i (read a, read b))
          . zip [0 ..]
          . map (splitOn ", ")
          . lines
          $ input
      b0 = boundingRect coords
      b1 = slightlyBigger b0
      g0 = computeGrid coords b0
      g1 = computeGrid coords b1
      g  = filter (\((_, n), (_, n')) -> n == n')
        $ zip (Map.toAscList g0) (Map.toAscList g1)

  print . snd . maximumBy (compare `on` snd) . map fst $ g -- Task A

boundingRect :: [Location] -> Rect
boundingRect = foldl f (Rect (1000000, 1000000) (0, 0))
 where
  f (Rect (x, y) (x', y')) (Location _ (nx, ny)) =
    Rect (min x nx, min y ny) (max x' nx, max y' ny)

slightlyBigger :: Rect -> Rect
slightlyBigger (Rect (x, y) (x', y')) = Rect (x - 1, y - 1) (x' + 1, y' + 1)

computeGrid :: [Location] -> Rect -> Grid
computeGrid pts (Rect (x, y) (x', y')) = foldl
  update'
  Map.empty
  [ (x, y) | x <- [x .. x'], y <- [y .. y'] ]
 where
  update' m coord = Map.alter f (closestTo coord) m
  f old = case old of
    Nothing -> Just 1
    Just n  -> Just (n + 1)
  closestTo coord =
    extractLocation
      . groupBy ((==) `on` snd)
      . sortBy (compare `on` snd)
      . zip pts
      . map (manhattan coord)
      $ pts
  extractLocation (ls : _) =
    if length ls > 1 then -1 else (\(Location id' _, _) -> id') . head $ ls

manhattan :: (Int, Int) -> Location -> Int
manhattan (x, y) (Location _ (x', y')) = abs (x - x') + abs (y - y')
