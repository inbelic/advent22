module D12.D12 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)
import Data.Char (ord)
import Data.List (transpose, sortBy)

type Posn = (Int, Int)
type Visited = [Posn]
type Queue = [Posn]

--wd = 8
--ht = 5
--
--start = (0, 0)
--end = (5, 2)

wd = 178
ht = 40

end = (154,20)
start = (0, 20)

type Map = [[Int]]

load :: String -> Map
load = map (map (f . flip (-) 97 . ord)) . lines
  where f (-14) = -1
        f (-28) = 26
        f x = x

getNextPosn :: Map -> Visited -> Posn -> Queue -> Queue
getNextPosn m vs (x, y) q
  = flip (++) q
  . filter (not . flip elem q)                -- ensure not already queued
  . filter (not . flip elem vs)               -- ensure not already visited
  . filter reachable                          -- ensure only one step up/down
  . filter within                             -- ensure on the grid
  $ [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
    where within (x, y)
            | x < 0 || wd <= x = False
            | y < 0 || ht <= y = False
            | otherwise = True

          curH = (m !! y) !! x 
          reachable (x, y) = let h = ((m !! y) !! x)
                              in h <= curH + 1

search :: Map -> Visited -> Queue -> Int -> Maybe Int
search m vs q cnt
  | elem end vs = Just cnt
  | otherwise = case foldr (getNextPosn m vs) [] q of
                  [] -> Nothing
                  nq -> search m (nq ++ vs) nq (cnt + 1)

getNextPosn' :: Map -> Visited -> Posn -> (Bool, Queue) -> (Bool, Queue)
getNextPosn' _ _ _ (True, _) = (True, [])
getNextPosn' m vs (x, y) (_, q)
  = fmap (flip (++) q)
  . (\ps -> (any isBottom ps, ps))
  . filter (not . flip elem q)                -- ensure not already queued
  . filter (not . flip elem vs)               -- ensure not already visited
  . filter reachable                          -- ensure only one step up/down
  . filter within                             -- ensure on the grid
  $ [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
    where within (x, y)
            | x < 0 || wd <= x = False
            | y < 0 || ht <= y = False
            | otherwise = True

          curH = (m !! y) !! x 
          reachable (x, y) = let h = ((m !! y) !! x)
                              in curH <= h + 1

          isBottom (x, y) = 0 == ((m !! y) !! x)

search' :: Map -> Visited -> Queue -> Int -> Maybe Int
search' m vs q cnt
  = case foldr (getNextPosn' m vs) (False, []) q of
      (True, _) -> Just (cnt + 1)
      (False, []) -> Nothing
      (False, nq) -> search' m (nq ++ vs) nq (cnt + 1)

run :: IO ()
run = do
  putStrLn $ "Day Twelve solutions are..."
  m <- liftM load $ readFile "app/D12/map.txt"
  putStrLn . show . search m [] [start] $ 0
  putStrLn . show . search' m [] [end] $ 0
