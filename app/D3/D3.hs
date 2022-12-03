module D3.D3 where

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.Char (ord)

sameUnique :: (Eq a) => [a] -> a -> [a] -> [a]
sameUnique ref cur acc
  | elem cur ref && (not . elem cur $ acc) = cur : acc
  | otherwise = acc

solve :: String -> Int
solve str = sum . map toPrio . foldr (sameUnique front) [] $ back
  where hl = flip div 2 . length $ str
        (front, back) = splitAt hl str

sameUnique' :: (Eq a) => [a] -> [a] -> [a]
sameUnique' cur acc = foldr (sameUnique acc) [] cur

solve' :: [String] -> Int
solve' (x:xs) = sum . map toPrio . foldr sameUnique' x $ xs

toPrio :: Char -> Int
toPrio x = f . ord $ x
  where f x
          | x <= 90 = x - 38
          | otherwise = x - 96

chunk :: Int -> [a] -> [[a]]
chunk x xs = chunk' x xs []

chunk' :: Int -> [a] -> [[a]] -> [[a]]
chunk' x [] acc = acc
chunk' x xs acc = chunk' x (drop x xs) (take x xs : acc)

run :: IO ()
run = do
  putStrLn "Day Three solutions are..."
  contents <- readFile "app/D3/rucksack.txt"
  putStrLn . show . sum . map solve . lines $ contents
  putStrLn . show . sum . map solve' . chunk 3 . lines $ contents
