module D2.D2 where

import Parser
import Control.Applicative 
import Control.Monad (liftM)

data Play
  = Rock
  | Paper
  | Sax
  deriving (Eq, Enum, Show, Ord)

rockP :: Parser Play
rockP = fmap (const Rock) (stringP "A" <|> stringP "X")

paperP :: Parser Play
paperP = fmap (const Paper) (stringP "B" <|> stringP "Y")

saxP :: Parser Play
saxP = fmap (const Sax) (stringP "C" <|> stringP "Z")

parsePlay :: Parser Play
parsePlay = rockP <|> paperP <|> saxP

roundP :: Parser (Play, Play)
roundP = filterCombinator (,) ((==) ' ') parsePlay parsePlay

load :: Parser [(Play, Play)]
load = linesP roundP

run :: IO ()
run = do
  putStrLn "Day Two solutions are..."
  contents <- readFile $ "app/D2/rps.txt"
  let (Just xs) = liftM snd . parse load $ contents
  putStrLn . show . solve $ xs
  putStrLn . show . solve' $ xs

score :: Play -> Play -> Int
score opp us = 1 + (fromEnum us) + (clash opp us)
  where
    clash :: Play -> Play -> Int
    clash Rock Paper = 6
    clash Paper Sax = 6
    clash Sax Rock = 6
    clash p1 p2
      | p1 == p2 = 3
      | otherwise = 0

solve :: [(Play, Play)] -> Int
solve = sum . map (uncurry score)

score' :: Play -> Play -> Int
score' opp Rock = score opp (lose opp)
  where lose Rock = Sax
        lose Sax = Paper
        lose Paper = Rock
score' opp Paper = score opp opp
score' opp Sax = score opp (win opp)
  where win Rock = Paper
        win Paper = Sax
        win Sax = Rock

solve' :: [(Play, Play)] -> Int
solve' = sum . map (uncurry score')
