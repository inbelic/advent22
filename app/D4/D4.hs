module D4.D4 where

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.Char (ord)

type Range = (Int, Int)

rangeP :: Parser Range
rangeP = filterCombinator (,) ((==) '-') intP intP

lineP :: Parser (Range, Range)
lineP = filterCombinator (,) ((==) ',') rangeP rangeP

loadP :: Parser [(Range, Range)]
loadP = linesP lineP

overlap :: Bool -> (Range, Range) -> Bool
overlap swap ((x1,x2), (y1,y2))
  | x1 <= y1 && x2 >= y2 = True
  | swap = overlap False ((y1,y2), (x1,x2))
  | otherwise = False

overlap' :: Bool -> (Range, Range) -> Bool
overlap' swap ((x1,x2), (y1,y2))
  | x1 <= y1 && x2 >= y1 = True
  | x1 <= y2 && x2 >= y2 = True
  | swap = overlap' False ((y1,y2), (x1,x2))
  | otherwise = False

run :: IO ()
run = do
  putStrLn "Day Four solutions are..."
  contents <- readFile "app/D4/cln.txt"
  let (Just xs) = fmap snd . parse loadP $ contents
  putStrLn . show . length . filter (overlap True) $ xs
  putStrLn . show . length . filter (overlap' True) $ xs
