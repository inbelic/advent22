module D9.D9 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)

data Dir
  = U
  | D
  | R
  | L
  deriving Show

dirP :: Dir -> Parser [Dir]
dirP d = fmap (flip take (repeat d)) p
  where p = stringP (show d ++ " ") *> intP

loadP :: Parser [Dir]
loadP = fmap concat . linesP $ p
  where p = dirP U <|> dirP D <|> dirP R <|> dirP L

type Posn = (Int, Int)
type Visited = [Posn]

updatePosn :: Dir -> Posn -> Posn
updatePosn U (x,y) = (x, y + 1)
updatePosn D (x,y) = (x, y - 1)
updatePosn R (x,y) = (x + 1, y)
updatePosn L (x,y) = (x - 1, y)

updateTail :: Posn -> Posn -> Posn
updateTail hp@(hx, hy) tp@(tx, ty)
  | tx > hx + 1 = updateDiag True hp . updatePosn L $ tp
  | tx < hx - 1 = updateDiag True hp . updatePosn R $ tp
  | ty > hy + 1 = updateDiag False hp . updatePosn D $ tp
  | ty < hy - 1 = updateDiag False hp . updatePosn U $ tp
  | otherwise = tp

updateDiag :: Bool -> Posn -> Posn -> Posn
updateDiag b (hx, hy) tp@(tx, ty)
  | (tx > hx) && (not b) = updatePosn L $ tp
  | (tx < hx) && (not b) = updatePosn R $ tp
  | (ty > hy) && b       = updatePosn D $ tp
  | (ty < hy) && b       = updatePosn U $ tp
  | otherwise = tp

updateVisited :: Posn -> Visited -> Visited
updateVisited p vs 
  | elem p vs = vs
  | otherwise = p : vs

move :: Dir -> (Posn, Posn, Visited) -> (Posn, Posn, Visited)
move d (hp, tp, vs) = (hp', tp', vs')
  where hp' = updatePosn d hp
        tp' = updateTail hp' tp
        vs' = updateVisited tp' vs

solve :: [Dir] -> Visited
solve = (\(_, _, vs) -> vs) . foldr move ((0, 0), (0, 0), [])

move' :: Dir -> ([Posn], Visited) -> ([Posn], Visited)
move' d (hp:tails, vs) = (hp':tails', vs')
  where hp' = updatePosn d hp
        tails' = chain hp' tails
        vs' = updateVisited (last tails') vs

chain :: Posn -> [Posn] -> [Posn]
chain _ [] = []
chain link (x:xs) = x' : chain x' xs
  where x' = updateTail link x

solve' :: Int -> [Dir] -> Visited
solve' len = (\(_, vs) -> vs) . foldr move' (take len (repeat (0,0)), [])

run :: IO ()
run = do
  putStrLn "Day Nine solutions are..."
  contents <- readFile "app/D9/moves.txt"
  let (Just ([], dirs)) = parse loadP contents
  putStrLn . show . length . solve . reverse $ dirs
  putStrLn . show . length . solve' 10 . reverse $ dirs
