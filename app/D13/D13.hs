module D13.D13 where 

import Parser
import Control.Applicative
import Control.Monad (liftM)
import Data.List (sort)

data Nest a
  = Bot a 
  | Mid [Nest a]
  deriving Show

scopeP :: Parser String
scopeP = Parser (scope 0)

-- gets string between two [] characters at the same level
scope :: Int -> String -> Maybe (String, String)
scope x ('[':str)
  | x == 0 = scope (x + 1) str
  | otherwise = liftM (fmap ('[' :)) . scope (x + 1) $ str
scope x (']':str)
  | x == 1    = Just (str, "")
  | otherwise = liftM (fmap (']' :)) . scope (x - 1) $ str
scope x (c:str) = liftM (fmap (c :)) . scope x $ str
scope _ _ = Nothing

botP :: Parser (Nest Int)
botP = fmap Bot intP

nestP :: Parser (Nest Int)
nestP = fmap (Mid . extrct . parse nestedP) scopeP
    where extrct (Just ([], x)) = x

nestedP :: Parser [Nest Int]
nestedP = sepP ',' (botP <|> nestP)


-- define our sorting stuff
instance (Ord a) => Eq (Nest a) where
  (==) n1 n2 = compNest n1 n2 == Nothing

instance (Ord a) => Ord (Nest a) where
  (<=) n1 n2 = case compNest n1 n2 of
                 (Just False) -> False
                 _ -> True

compNest :: (Ord a) => Nest a -> Nest a -> Maybe Bool
compNest (Bot x) (Bot y)
  | x == y    = Nothing
  | otherwise = Just (x < y)
compNest (Mid xs) (Bot y) = compNest (Mid xs) (Mid [Bot y])
compNest (Bot x) (Mid ys) = compNest (Mid [Bot x]) (Mid ys)
compNest (Mid []) (Mid []) = Nothing
compNest (Mid _) (Mid []) = Just False
compNest (Mid []) (Mid _) = Just True
compNest (Mid (x:xs)) (Mid (y:ys))
  = case compNest x y of
      Nothing -> compNest (Mid xs) (Mid ys)
      result -> result

-- solving glue
solve :: [(Int, (Nest Int, Nest Int))] -> Int
solve [] = 0
solve ((idx, (x,y)):xs) = f x y + solve xs
  where f x y = case compNest x y of
                  (Just False) -> 0
                  _ -> idx

solve' :: [Nest Int] -> Int
solve' = foldr (*) 1 . map fst . filter (\(_, x) -> x == start || x == end)
       . zip [1..] . sort . (:) start . (:) end
  where (Just (_,end)) = parse nestP "[[6]]"
        (Just (_,start)) = parse nestP "[[2]]"

chunk :: Int -> [a] -> [[a]]
chunk x xs = chunk' x xs []

chunk' :: Int -> [a] -> [[a]] -> [[a]]
chunk' x [] acc = acc
chunk' x xs acc = chunk' x (drop x xs) (take x xs : acc)

load :: String -> [(Nest Int, Nest Int)]
load = map (split . map (extrct . parse nestP) . take 2) . chunk 3 . lines
  where extrct (Just (_, n)) = n
        split (x:y:[]) = (x,y)

run :: IO ()
run = do
  putStrLn "Day Thirteen solutions are..."
  contents <- readFile "app/D13/packets.txt" 
  putStrLn . show . solve . zip [1..] . reverse . load $ contents
  putStrLn . show . solve' . (\(xs, ys) -> xs ++ ys) . unzip . load $ contents
