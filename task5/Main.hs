module Main where

import           Data.Char
import           Data.List
import           Data.Function

main :: IO ()
main = do
  input <- getContents
  print . length . process $ input -- Task A
  print
    . minimumBy (compare `on` snd)
    . map (\l -> (l, length . process . removePolymer input $ l))
    $ ['a' .. 'z'] -- Task B

process :: String -> String
process xs = if xs == xs' then xs' else process xs'
 where
  process' []             = []
  process' [  a         ] = [a]
  process' s@(a : b : xs) = if a /= b && toLower a == toLower b
    then process' xs
    else a : process' (b : xs)

  xs' = process' xs

removePolymer :: String -> Char -> String
removePolymer s c = filter (\a -> toLower a /= c) s
