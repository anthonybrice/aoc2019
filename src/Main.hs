module Main where

import System.Environment (getArgs)
import Data.Digits (digits)
import Data.Maybe (fromJust)
import Data.List (elemIndex, find, group, mapAccumL)
import Data.List.Index (setAt)
import Data.List.Split (splitOn)
import Data.Set (intersection, fromList, toList)

main :: IO ()
main = do
  arg:_ <- getArgs
  case arg of
    "d1p1" -> d1p1
    "d1p2" -> d1p2
    "d2p1" -> d2p1
    "d2p2" -> d2p2
    "d3p1" -> d3p1
    "d3p2" -> d3p2
    "d4p1" -> d4p1
    "d4p2" -> d4p2

d4p2 :: IO ()
d4p2 = do
  let ns = [152085..670283]
      containsUniquePair n =
        let ns = digits 10 n
        in not . null . filter (\x -> length x == 2) $ group ns
      pws = filter containsUniquePair $ filter isAscending ns
  putStrLn . show $ length pws

isAscending n =
  let d:ds = digits 10 n
      f p (x:xs) = if p > x then False else f x xs
      f _ [] = True
  in f d ds

d4p1 :: IO ()
d4p1 = do
  let ns = [152085..670283]
      isTwoAdjacentSame n =
        let d:ds = digits 10 n
            f p (x:xs) = if p == x then True else f x xs
            f _ [] = False
        in f d ds
      pws = filter isAscending $ filter isTwoAdjacentSame ns
  putStrLn . show $ length pws

d3p2 :: IO ()
d3p2 = do
  rawInput <- readFile "input3"
  let paths = map (splitOn ",") $ lines rawInput
      [wire1, wire2] = map makePath paths
      crossings = toList $ fromList wire1 `intersection` fromList wire2
      d = 2 + (foldl1 min
                $ map (\p -> fromJust (elemIndex p wire1)
                        + fromJust (elemIndex p wire2)) crossings)
  putStrLn $ show d

d3p1 :: IO ()
d3p1 = do
  rawInput <- readFile "input3"
  let paths = map (splitOn ",") $ lines rawInput
      [wire1, wire2] = map makePath paths
      crossings = toList $ fromList wire1 `intersection` fromList wire2
      d = foldl1 min $ map (\(x,y) -> abs x + abs y) crossings
  putStrLn $ show d

makePath :: [String] -> [(Integer, Integer)]
makePath moves =
  let doMove (x,y) (d:m) =
        let n = read m :: Integer
        in case d of
          'R' -> [(x',y) | x' <- [(x+1)..(x+n)]]
          'L' -> reverse [(x',y) | x' <- [(x-n)..(x-1)]]
          'U' -> [(x,y') | y' <- [(y+1)..(y+n)]]
          'D' -> reverse [(x,y') | y' <- [(y-n)..(y-1)]]
  in concat . snd $ mapAccumL
       (\p m -> let l = doMove p m in (last l, l))
       (0,0) moves

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
  else case n of 99 -> findNums xs (m + 1) 0
                 _ -> findNums xs m (n + 1)

isNums :: [Int] -> Int -> Int -> Bool
isNums (x:xs) m n =
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
    1 -> doOp (+)
    2 -> doOp (*)
  where
    doOp b = compute (setAt
                       (xs!!(i+3))
                       ((xs!!(xs!!(i+1)))
                         `b` (xs!!(xs!!(i+2))))
                       xs) (i+4)

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
