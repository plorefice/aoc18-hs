module Main where

import           Data.List
import           Data.Function

main :: IO ()
main = do
  input <- getContents
  print . uncurry (*) . foldl countIDs (0, 0) . lines $ input

countIDs :: (Int, Int) -> String -> (Int, Int)
countIDs (n2, n3) id = (n2 + f 2, n3 + f 3)
  where f n = min 1 . length . filter ((== n) . length) . group . sort $ id
