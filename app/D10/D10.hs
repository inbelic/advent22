module D10.D10 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)

type Value = Int
type Cycle = Int

data Cmd
  = Skip
  | Now Int
  | Soon Int
  deriving (Show)

type State = ([Cmd], Cycle, Value, Value)

skipP :: Parser Cmd
skipP = fmap (const Skip) $ stringP "noop"

soonP :: Parser Cmd
soonP = fmap Soon (stringP "addx " *> intP)

cmdP :: Parser Cmd
cmdP = skipP <|> soonP

loadP :: Parser [Cmd]
loadP = linesP cmdP

comp :: State -> State
comp (cmds, cyc, cur, ttl)
  | mod (cyc - 20) 40 == 0 = comp' (cmds, cyc, cur, ttl + (cyc * cur))
  | otherwise = comp' (cmds, cyc, cur, ttl)

comp' :: State -> State
comp' ([], cyc, cur, ttl) = ([], cyc, cur, ttl)
comp' (cmd:cmds, cyc, cur, ttl)
  = case cmd of
      Skip -> comp (cmds, cyc + 1, cur, ttl)
      (Soon x) -> comp (Skip:(Now x):cmds, cyc + 1, cur, ttl)
      (Now x) -> comp' (cmds, cyc, cur + x, ttl)

solve :: [Cmd] -> State
solve cmds = comp (cmds, 0, 1, 0)

type State' = ([Cmd], Cycle, [Value])

record :: State' -> State'
record ([], cyc, vals) = ([], cyc, vals)
record (cmd:cmds, cyc, (cur:vals))
  = case cmd of
      Skip -> record (cmds, cyc + 1, cur:cur:vals)
      (Soon x) -> record (Skip:(Now x):cmds, cyc + 1, cur:cur:vals)
      (Now x) -> record (cmds, cyc, (cur + x):vals)

solve' :: [Cmd] -> State'
solve' cmds = record (cmds, 0, [1])

draw :: Int -> (Int, [Bool]) -> (Int, [Bool])
draw x (l, v)
  | (<) 1 $ abs (l - x) = (l + 1, False:v)
  | otherwise = (l + 1, True:v)

chunk :: Int -> [a] -> [[a]]
chunk amt xs
  | length x == amt = (:) x . chunk amt . drop amt $ xs
  | otherwise = []
  where x = take amt xs

output :: [[Bool]] -> String
output [] = ""
output (x:xs) = output' x ++ output xs

output' :: [Bool] -> String
output' = flip (++) "\n" . concat . map f
  where f True = "#"
        f False = "."

run :: IO ()
run = do
  putStrLn "Day Ten solutions are..."
  contents <- readFile "app/D10/instructions.txt"
  let (Just ([], cmds)) = parse loadP contents
  putStrLn . show . (\(_,_,_,ttl) -> ttl) . solve $ cmds
  putStrLn . output . map (reverse . snd . foldr draw (0,[]) . reverse)
    . chunk 40 . reverse . (\(_,_,vals) -> vals) . solve' $ cmds

test = [Skip, Soon 3, Soon (-5)]
