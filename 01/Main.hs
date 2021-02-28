module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let nums :: [Int]
      nums = map read args
  putStrLn . show $ foldl (+) 0 nums
