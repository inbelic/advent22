module D11.D11 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)
import Data.List (sort)

data Monkey = Monkey
  { items :: [Int]
  , oper  :: Int -> Int
  , test  :: Int -> Int
  , count :: Int
  }

instance Show Monkey where
      show (Monkey i _ _ c) = show i

m0 = Monkey
      [54, 61, 97, 63, 74]
      (* 7)
      (\x -> case mod x 17 of
               0 -> 5
               _ -> 3)
      0

m1 = Monkey
      [61, 70, 97, 64, 99, 83, 52, 87]
      (+ 8)
      (\x -> case mod x 2 of
               0 -> 7
               _ -> 6)
      0

m2 = Monkey
      [60, 67, 80, 65]
      (* 13)
      (\x -> case mod x 5 of
               0 -> 1
               _ -> 6)
      0

m3 = Monkey
      [61, 70, 76, 69, 82, 56]
      (+ 7)
      (\x -> case mod x 3 of
               0 -> 5
               _ -> 2)
      0

m4 = Monkey
      [79, 98]
      (+ 2)
      (\x -> case mod x 7 of
               0 -> 0
               _ -> 3)
      0

m5 = Monkey
      [72, 79, 55]
      (+ 1)
      (\x -> case mod x 13 of
               0 -> 2
               _ -> 1)
      0

m6 = Monkey
      [63]
      (+ 4)
      (\x -> case mod x 19 of
               0 -> 7
               _ -> 4)
      0

m7 = Monkey
      [72, 51, 93, 63, 80, 86, 81]
      (\x -> x * x)
      (\x -> case mod x 11 of
               0 -> 0
               _ -> 4)
      0

ttlDivs = 11 * 19 * 13 * 7 * 3 * 5 * 2 * 17

track :: Monkey -> Monkey
track (Monkey i o t cnt) = Monkey [] o t (cnt + length i)

catch :: Throw -> [Monkey] -> [Monkey]
catch (to, val) = modify to (catch' val)

catch' :: Int -> Monkey -> Monkey
catch' val (Monkey i o t cnt) = Monkey (val : i) o t cnt

type Throw = (Int, Int)

play :: Monkey -> ([Throw], Monkey)
play m = (throws, track m)
  where cnt' = length . items $ m
        throws = foldr (inspect m) [] . items $ m

inspect :: Monkey -> Int -> [Throw] -> [Throw]
inspect m x acc = (test m x', x') : acc
  where x' = flip div 3 . (oper m) $ x

doRound :: Int -> [Monkey] -> [Monkey]
doRound x m
  | length m <= x = m
  | otherwise = doRound (x + 1) . modify x (const cur') $ m'
  where cur = m !! x
        (throws, cur') = play cur
        m' = foldr catch m throws

play' :: Monkey -> ([Throw], Monkey)
play' m = (throws, track m)
  where cnt' = length . items $ m
        throws = foldr (inspect' m) [] . items $ m

inspect' :: Monkey -> Int -> [Throw] -> [Throw]
inspect' m x acc = (test m x', x') : acc
  where x' = flip mod ttlDivs . (oper m) $ x

doRound' :: Int -> [Monkey] -> [Monkey]
doRound' x m
  | length m <= x = m
  | otherwise = doRound' (x + 1) . modify x (const cur') $ m'
  where cur = m !! x
        (throws, cur') = play' cur
        m' = foldr catch m throws

modify :: Int -> (a -> a) -> [a] -> [a]
modify idx f = map ifIdx . zip [0..]
      where ifIdx (cur, val)
              | cur == idx = f val
              | otherwise = val

doX :: Int -> (a -> a) -> a -> a
doX 0 f = id
doX x f = doX (x - 1) f . f

run :: IO ()
run = do
  putStrLn "Day Eleven solutions are..."
  let m = [m0,m1,m2,m3,m4,m5,m6,m7]
  putStrLn . show . foldr (*) 1 . take 2 . reverse . sort . map count . doX 20 (doRound 0) $ m
  putStrLn . show . foldr (*) 1 . take 2 . reverse . sort . map count . doX 10000 (doRound' 0) $ m
