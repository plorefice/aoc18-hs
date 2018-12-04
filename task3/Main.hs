module Main where

import           Lib
import           Text.Megaparsec
import           Data.List

data Fabric = Fabric Int (Int, Int) (Int, Int) deriving (Eq, Show)

main :: IO ()
main = do
  input <- getContents
  let fabric = map parseFabric . lines $ input
  print . overlapping $ fabric -- Task A
  print
    . filter ((== False) . snd)
    . map (\f@(Fabric id' _ _) -> (id', overlaps fabric f))
    $ fabric -- Task B

parseFabric :: String -> Fabric
parseFabric s = case parse fabric "" s of
  Right f -> f
  Left  e -> error $ show e
 where
  fabric :: Parser Fabric
  fabric = do
    sym "#"
    id' <- int
    sym "@"
    x <- int
    sym ","
    y <- int
    sym ":"
    w <- int
    sym "x"
    h <- int
    return $ Fabric id' (x, y) (w, h)

gridify :: Fabric -> [(Int, Int)]
gridify (Fabric _ (x, y) (w, h)) =
  [ (x', y') | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1] ]

overlapping :: [Fabric] -> Int
overlapping =
  length . filter ((> 1) . length) . group . sort . concatMap gridify

overlaps :: [Fabric] -> Fabric -> Bool
overlaps []       _  = False
overlaps (f : fs) f' = (f /= f' && overlaps' f f') || overlaps fs f'
 where
  overlaps' (Fabric _ (x, y) (w, h)) (Fabric _ (x', y') (w', h')) =
    x < (x' + w') && (x + w) > x' && y < (y' + h') && (y + h) > y'
