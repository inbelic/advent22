module D15.D15 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)
import Data.List (sort)

type Point = (Int, Int)

--             a, b
type Line = (Int, Int)    -- has the form y = ax + b
type Diamond = (Int, Point)
type Sensor = Point
type Beacon = Point

sensorP :: Parser Sensor
sensorP = combinator (,) xP yP
  where xP = stringP "Sensor at x=" *> intP
        yP = stringP ", y=" *> intP

beaconP :: Parser Beacon
beaconP = combinator (,) xP yP
  where xP = stringP " closest beacon is at x=" *> intP
        yP = stringP ", y=" *> intP

diamondP :: Parser Diamond
diamondP = filterCombinator mkDiamond ((==) ':') sensorP beaconP

load :: String -> [Diamond]
load str = case parse (linesP diamondP) str of
             Nothing -> []
             (Just ([], ds)) -> ds
             (Just _) -> []

mkDiamond :: Sensor -> Beacon -> Diamond
mkDiamond s b = (manDist s b, s)

mkLines :: Diamond -> (Line, Line, Line, Line)
mkLines (dd, (sx, sy))
  = ( mkLine True  (sx + dd, sy)
    , mkLine False (sx + dd, sy)
    , mkLine True  (sx - dd, sy)
    , mkLine False (sx - dd, sy)
    )

mkLine :: Bool -> Point -> Line
mkLine True (x, y) = (1, y - x)
mkLine False (x,y ) = (-1, x + y)

manDist :: Point -> Point -> Int
manDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

within :: Point -> Diamond -> Bool
within p (dd, ds) = manDist p ds <= dd

-- when lines are of the form y = ax + b
intersect :: Line -> Line -> Point -> Maybe Point
intersect (a1, b1) (a2, b2) (rx, _)
  | a1 == a2 = Nothing      -- parallel or same line
  | otherwise = do
    let nx = b2 - b1
        dx = a1 - a2
        x = innerDiv rx nx dx
        y = x * a1 + b1
    Just (x, y)

innerDiv :: Int -> Int -> Int -> Int
innerDiv r n d
  | mod n d == 0 = t 
  | 0 <= t - r = t
  | otherwise = t + 1
  where t = div n d

intersection :: Line -> Diamond -> Maybe (Point, Point)
intersection l d@(_, dp) = do
  let (l1, l2, l3, l4) = mkLines d
      i1 = intersect l l1 dp
      i2 = intersect l l2 dp
      i3 = intersect l l3 dp
      i4 = intersect l l4 dp
      (p1, p2) = middlePair [i1,i2,i3,i4]
  case within p1 d && within p2 d of
    False -> Nothing
    True -> Just (p1, p2)

middlePair :: [Maybe Point] -> (Point, Point)
middlePair ps
  = case sort ps of
      (Nothing:Nothing:(Just m1):(Just m2):[]) -> (m1, m2)
      (_:(Just m1):(Just m2):_:[]) -> (m1, m2)

collapse :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
collapse x [] = [x]
collapse x (hd:tl)
  = case (compare (snd x) (fst hd), collapse' x hd) of
      (_, Just x') -> collapse x' tl
      (LT, Nothing) -> x : (hd : tl)
      (_, Nothing) -> hd : collapse x tl

collapse' :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
collapse' (s1, e1) (s2, e2)
  | e1 + 1 < s2 = Nothing
  | e2 + 1 < s1 = Nothing
  | otherwise = Just (min s1 s2, max e1 e2)

range :: [(Int, Int)] -> Int
range = sum . map (abs . uncurry (-))

solve :: (Point -> Int) -> Line -> [Diamond] -> [(Int, Int)]
solve xtrct l = foldr f [] . map (intersection l)
  where f Nothing acc = acc
        f (Just (p1, p2)) acc = collapse (xtrct p1, xtrct p2) acc

mkOffsetLines :: Diamond -> [Line]
mkOffsetLines (dd, (sx, sy))
  = [ mkLine True  (sx + dd + 1, sy)
    , mkLine False (sx + dd + 1, sy)
    , mkLine True  (sx - dd - 1, sy)
    , mkLine False (sx - dd - 1, sy)
    ]

solve' :: (Int, Int) -> [Diamond] -> Maybe Point
solve' rng ds = do
  col <- foldr hasEmpty Nothing . map (\l -> solve fst l ds) $ perms
  row <- flip hasEmpty Nothing . solve fst (0, col) . map swap $ ds
  Just (col, row)
  where perms = concatMap mkOffsetLines $ ds 
        hasEmpty _ (Just p) = Just p
        hasEmpty xs acc
          | length xs <= 1 = acc
          | length xs' <= 1 = acc
          | otherwise = Just . (+ 1) . snd . head $ xs'
             where xs' = foldr (trim rng) [] xs

        swap (dd, (dx, dy)) = (dd, (dy, dx))

trim :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
trim (ms, me) (cs, ce) acc
  | ce <= ms = acc
  | me <= cs = acc
  | otherwise = (max ms cs, min me ce) : acc

toFreq :: Point -> Int
toFreq (x, y) = 4000000 * x + y

run :: IO ()
run = do
  putStrLn "Day Fifteen solutions are..."
  ds <- liftM load . readFile $ "app/D15/bacon.txt"
  putStrLn . show . range . solve fst (0, 10) $ ds
  putStrLn . show . liftM toFreq . solve' (0, 4000000) $ ds
