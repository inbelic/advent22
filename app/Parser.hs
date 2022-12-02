module Parser where

import Data.Char (isDigit)
import Control.Applicative 
import Control.Monad (liftM)

newtype Parser s = Parser
  { parse :: String -> Maybe (String, s)
  }

instance Functor Parser where
  fmap f (Parser p) 
    = Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', x) <- p2 input'
    Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | x == y = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    case null xs of
      True -> Nothing
      False -> Just (input', xs)

nintP :: Parser Int
nintP = negate <$> (charP '-' *> pintP)

pintP :: Parser Int
pintP = f <$> notNull (spanP isDigit)
  where f ds = read ds

intP :: Parser Int
intP = nintP <|> pintP

untilP :: (String -> Bool) -> Parser a -> Parser [a]
untilP cond p = Parser f
  where f input = case cond input of
                    True -> Just (input, [])
                    False -> do
                      (input', x) <- parse p input
                      liftM (fmap (x :)) . parse (untilP cond p) $ input'

sepP :: Char -> Parser a -> Parser [a]
sepP sep p = untilP null p'
  where p' = (p <* charP sep) <|> (charP sep *> p) <|> p

linesP :: Parser a -> Parser [a]
linesP p = sepP '\n' p

combinator :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
combinator comb (Parser p1) (Parser p2)
  = Parser $ \input -> do
    (input', x) <- p1 input
    (input'', y) <- p2 input'
    Just (input'', comb x y)

filterCombinator :: (a -> b -> c) -> (Char -> Bool) -> Parser a -> Parser b -> Parser c
filterCombinator comb filt (Parser p1) (Parser p2) =
  Parser $ \input -> do
    (input', x) <- p1 input
    let input'' = snd . splitWith filt $ input'
    (input''', y) <- p2 input''
    Just (input''', comb x y)

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith cond xs = churn cond ([], xs)

churn :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
churn _ (before, []) = (reverse before, [])
churn cond (before, (x:after))
  = case cond x of
      True -> (reverse before, after)
      False -> churn cond (x:before, after)

splitWhen :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
splitWhen cond [] acc = acc
splitWhen cond xs acc
  = splitWhen cond rest (token:acc)
    where (token, rest) = splitWith cond xs

bracketed :: Parser a -> Parser a
bracketed p = charP '(' *> p <* charP ')'

allowSpace :: Parser a -> Parser a
allowSpace p = charP ' ' *> p <|> p
