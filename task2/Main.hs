module Main where

import           Data.List
import           Data.Function

main :: IO ()
main = do
  input <- getContents
  let xs = lines input
  print . uncurry (*) . foldl countIDs (0, 0) $ xs -- Task A
  print
    . map fst
    . filter (uncurry (==))
    . uncurry zip
    . head
    . filter differByOne
    $ [ (a, b) | a <- xs, b <- xs, a /= b ] -- Task B

countIDs :: (Int, Int) -> String -> (Int, Int)
countIDs (n2, n3) id = (n2 + f 2, n3 + f 3)
  where f n = min 1 . length . filter ((== n) . length) . group . sort $ id

differByOne :: (String, String) -> Bool
differByOne (a, b) = (== 1) . length . filter (uncurry (/=)) $ zip a b
