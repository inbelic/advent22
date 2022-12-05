module Main where

import qualified D1.D1 as D1
import qualified D2.D2 as D2
import qualified D3.D3 as D3
import qualified D4.D4 as D4
import qualified D5.D5 as D5

main :: IO ()
main = do
  putStrLn "Hello, welcome to Finn's hackathon"
  putStrLn ""
  D1.run
  putStrLn ""
  D2.run
  putStrLn ""
  D3.run
  putStrLn ""
  D4.run
  putStrLn ""
  D5.run
  putStrLn ""
  putStrLn "That's all folks"
