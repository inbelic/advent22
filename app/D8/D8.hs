module D8.D8 where 

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.Char (ord)
import Data.List (transpose)

visible :: [(Bool, Int)] -> [(Bool, Int)]
visible = snd . foldr visible' (-1, [])

visible' :: (Bool, Int) -> (Int, [(Bool, Int)]) -> (Int, [(Bool, Int)])
visible' (vis, cur) (mx, xs)
  | cur > mx = (cur, (True, cur) : xs)
  | otherwise = (mx, (vis, cur) : xs)

solve :: [[(Bool, Int)]] -> [[(Bool, Int)]]
solve = f . transpose . f
  where f = map visible . map reverse . map visible

count :: [[(Bool, Int)]] -> Int
count = sum . map (length . filter fst)

scenic :: [(Int, Int)] -> [(Int, Int)]
scenic = foldr scenic' []

scenic' :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
scenic' (scr, cur) prev = (scr * strk, cur) : prev
  where strk = counter 0 cur . map snd $ prev

        counter :: Int -> Int -> [Int] -> Int
        counter cnt _ [] = cnt
        counter cnt x (y:ys)
          | x > y = counter (cnt + 1) x ys
          | otherwise = cnt + 1

solve' :: [[(Int, Int)]] -> [[(Int, Int)]]
solve' = f . transpose . f
  where f = map scenic . map reverse . map scenic

count' :: [[(Int, Int)]] -> Int
count' = maximum . map (maximum . map fst)

load :: [String] -> [[Int]]
load = map (map f)
  where f = flip (-) 48 . ord

run :: IO ()
run = do
  putStrLn "Day Eight solutions are..."
  contents <- liftM (load . lines) . readFile $ "app/D8/trees.txt"
  putStrLn . show . count . solve . map (zip (repeat False)) $ contents
  putStrLn . show . count' . solve' . map (zip (repeat 1)) $ contents
