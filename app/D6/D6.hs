module D6.D6 where 

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.List (transpose)

unique :: (Eq a) => [a] -> Bool
unique = unique' []

unique' :: (Eq a) => [a] -> [a] -> Bool
unique' _ [] = True
unique' ref (x:xs)
  | elem x ref = False
  | otherwise = unique' (x:ref) xs

iter :: Int -> [a] -> a -> [a]
iter size xs x = (:) x . take size $ xs

solve :: (Eq a) => Int -> Int -> [a] -> [a] -> Int
solve size posn _ [] = posn
solve size posn buf (x:xs)
  | unique buf' && posn > size = posn
  | otherwise = solve size (posn + 1) buf' xs
  where buf' = iter (size - 1) buf x

run :: IO ()
run = do
  putStrLn "Day Six solutions are..."
  contents <- readFile "app/D6/signal.txt"
  putStrLn . show . flip (solve 4 1) contents . take 4 . repeat $ 'z'
  putStrLn . show . flip (solve 14 1) contents . take 14 . repeat $ 'z'
