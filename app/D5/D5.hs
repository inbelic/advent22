module D5.D5 where

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.List (transpose)

data Square
  = Empty
  | Full Char
  deriving (Show, Eq)

type Grid = [[Square]]

emptyP :: Parser Square
emptyP = fmap (const Empty) (charP ' ' *> charP ' ' *> charP ' ')

fullP :: Parser Square
fullP = fmap Full (charP '[' *> oneChar <* charP ']')

squareP :: Parser Square
squareP = emptyP <|> fullP

rowP :: Parser [Square]
rowP = sepP ' ' squareP

cleanGrid :: Grid -> Grid
cleanGrid = map (dropWhile ((==) Empty))

data Move = Move
  { amnt  :: Int
  , from  :: Int
  , to    :: Int
  }
  deriving (Show)

amntP :: Parser Int
amntP = stringP "move " *> intP <* charP ' '

fromP :: Parser Int
fromP = stringP "from " *> intP <* charP ' '

toP :: Parser Int
toP = stringP "to " *> intP

moveP :: Parser Move
moveP = fmap toMove . combinator (,) amntP . combinator (,) fromP $ toP

toMove :: (Int, (Int, Int)) -> Move
toMove (x,(y,z)) = Move x y z

processStack :: Move -> Grid -> Grid
processStack mv grid = foldr processMove' grid . take (amnt mv) . repeat $ mv

processMove' :: Move -> Grid -> Grid
processMove' mv = (\(sqr, grid') -> push (to mv) sqr grid') . pop (from mv)

processQueue :: Move -> Grid -> Grid
processQueue mv grid = foldr (push (to mv)) grid' . reverse $ stack
  where (stack, grid') = foldr processPops ([], grid) 
                       . take (amnt mv) . repeat $ mv

processPops :: Move -> ([Square], Grid) -> ([Square], Grid)
processPops mv (sqrs, grid) = (sqr : sqrs, grid')
  where (sqr, grid') = pop (from mv) grid

push :: Int -> Square -> Grid -> Grid
push col Empty = id
push col sqr = map (push' col sqr) . zip [1..]

push' :: Int -> Square -> (Int, [Square]) -> [Square]
push' col sqr (posn, sqrs)
  | col /= posn = sqrs
  | otherwise = sqr : sqrs

pop :: Int -> Grid -> (Square, Grid)
pop col = (\(sqrs, grid) -> (head . drop (col - 1) $ sqrs, grid))
        . unzip . map (pop' col) . zip [1..]
  where f = (/=) Empty

pop' :: Int -> (Int, [Square]) -> (Square, [Square])
pop' col (posn, sqrs)
  | col /= posn = (Empty, sqrs)
  | otherwise = (sqr, sqrs')
  where (sqr:sqrs') = case sqrs of
                        [] -> [Empty]
                        xs -> xs

score :: Grid -> String
score = map (toScore . dropWhile ((==) Empty))
  where toScore [] = ' '
        toScore ((Full c):_) = c

run :: IO ()
run = do
  putStrLn "Day Five solutions are..."
  contents <- fmap lines . readFile $ "app/D5/moving.txt"
  let (Just start) = fmap (cleanGrid . transpose) . sequence
                   . map (fmap snd . parse rowP)
                   . take 8 $ contents
  let (Just moves) = sequence . map (fmap snd . parse moveP)
                   . drop 10 $ contents
  putStrLn . score . foldr processStack start . reverse $ moves
  putStrLn . score . foldr processQueue start . reverse $ moves
