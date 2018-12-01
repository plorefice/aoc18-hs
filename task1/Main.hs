module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

main :: IO ()
main = do
  input <- getContents
  let xs = map read' . lines $ input :: [Int]
  print $ sum xs -- Task A
  print $ fst . foldlUntil firstDup (0, Set.empty) . scanl (+) 0 $ cycle xs -- Task B

read' :: Read a => String -> a
read' ('+':xs) = read xs
read' x = read x

firstDup :: Ord a => (a, Set a) -> a -> Either (a, Set a) (a, Set a)
firstDup (n, s) n'
  | s == s'   = Left  (n', s)
  | otherwise = Right (n', s')
  where s' = Set.insert n' s

foldlUntil :: Foldable t => (b -> a -> Either b b) -> b -> t a -> b
foldlUntil f z t = either id id $ foldM f z t
