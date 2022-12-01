module Main where

import qualified D1.D1 as D1

main :: IO ()
main = do
  putStrLn "Hello, welcome to Finn's hackathon"
  D1.run
  putStrLn "That's all folks"
