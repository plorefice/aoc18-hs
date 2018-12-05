module Main where

import           Data.Char
import           Debug.Trace

main :: IO ()
main = do
  input <- getContents
  print . length . process $ input

process :: String -> String
process xs = if xs == xs' then xs' else process xs'
 where
  process' []             = []
  process' [  a         ] = [a]
  process' s@(a : b : xs) = if a /= b && toLower a == toLower b
    then process' xs
    else a : process' (b : xs)

  xs' = process' xs
