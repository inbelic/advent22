module D1.D1 where

import Parser
import Control.Monad

load :: Parser Int
load = Parser $ f
  where f "" = Just ("", 0)
        f str = parse pintP str

run :: IO ()
run = do
  putStrLn "Day One solutions are..."
  contents <- liftM lines . readFile $ "app/D1/jungle.txt"
  let (Just xs) = sequence . map (fmap snd . parse load) $ contents
  putStrLn . show . snd . foldr solve (0,0) $ xs
  putStrLn . show . sum . snd . foldr solve' (0, [0,0,0]) $ xs

solve :: Int -> (Int, Int) -> (Int, Int)
solve 0 (cur, best)
  | cur > best = (0, cur)
  | otherwise = (0, best)
solve x (cur, best) = (x + cur, best)

solve' :: Int -> (Int, [Int]) -> (Int, [Int])
solve' 0 (cur, best)
  | sum newCur > (sum best) = (0, newCur)
  | otherwise = (0, best)
    where newCur = maxim cur best
solve' x (cur, best) = (x + cur, best)

maxim :: Int -> [Int] -> [Int]
maxim _ [] = []
maxim y (x:xs)
  | y > x = y : xs
  | otherwise = x : (maxim y xs)
