module Main where

import qualified D1.D1 as D1
import qualified D2.D2 as D2
import qualified D3.D3 as D3
import qualified D4.D4 as D4
import qualified D5.D5 as D5
import qualified D6.D6 as D6
import qualified D7.D7 as D7
import qualified D8.D8 as D8

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
  D6.run
  putStrLn ""
  D7.run
  putStrLn ""
  D8.run
  putStrLn ""
  putStrLn "That's all folks"
