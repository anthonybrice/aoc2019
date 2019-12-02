module Main where

import System.Environment
import Data.List.Split (splitOn)

main :: IO ()
main = do
  args <- getArgs
  case args!!0 of
    "d1p1" -> d1p1
    "d1p2" -> d1p2
    "d2p1" -> d2p1
    "d2p2" -> d2p2

d2p2 :: IO ()
d2p2 = do
  rawInput <- readFile "input2"
  let input = map read $ splitOn "," rawInput :: [Int]
      (m, n) = findNums input 0 0
  putStrLn $ show $ 100 * m + n

findNums :: [Int] -> Int -> Int -> (Int, Int)
findNums xs m n =
  if isNums xs m n
  then (m, n)
  else
    case n of
      99 -> findNums xs (m+1) 0
      _ -> findNums xs m (n+1)

isNums :: [Int] -> Int -> Int -> Bool
isNums l@(x:xs) m n =
  let xs' = x : m : n : (tail $ tail xs)
  in (compute xs' 0)!!0 == 19690720

d2p1 :: IO ()
d2p1 = do
  rawInput <- readFile "input2"
  let input = map read $ splitOn "," rawInput :: [Int]
      computed = compute input 0
  putStrLn $ show $ computed!!0

compute :: [Int] -> Int -> [Int]
compute xs i =
  case xs!!i of
    99 -> xs
    1 -> compute (replaceNth (xs!!(i+3)) ((xs!!(xs!!(i+1))) + (xs!!(xs!!(i+2)))) xs) (i+4)
    2 -> compute (replaceNth (xs!!(i+3)) ((xs!!(xs!!(i+1))) * (xs!!(xs!!(i+2)))) xs) (i+4)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

d1p1 :: IO ()
d1p1 = do
  rawInput <- readFile "input"
  let input = lines rawInput
      ns = map read input :: [Integer]
      sum' = sum $ map (\n -> n `div` 3 - 2) ns
  putStrLn $ show sum'

d1p2 :: IO ()
d1p2 = do
  rawInput <- readFile "input"
  let input = lines rawInput
      ns = map read input :: [Integer]
      sum' = sum $ map totalFuel ns
  putStrLn $ show sum'

totalFuel :: Integer -> Integer
totalFuel x =
  let initFuel = x `div` 3 - 2
  in if initFuel >= 1
     then initFuel + totalFuel initFuel
     else 0
