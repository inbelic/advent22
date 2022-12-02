module Main where

import qualified D1.D1 as D1
import qualified D2.D2 as D2

main :: IO ()
main = do
  putStrLn "Hello, welcome to Finn's hackathon"
  putStrLn ""
  D1.run
  putStrLn ""
  D2.run
  putStrLn ""
  putStrLn "That's all folks"
