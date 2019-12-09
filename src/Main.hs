{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Digits (digits)
import Data.Graph (dfs, graphFromEdges, Vertex)
--import Data.Graph.UGraph (UGraph)
--import Data.Graph.Types (Edge)
import Data.Array.IArray
import Data.Maybe (fromJust, mapMaybe)
import Data.List (elemIndex, find, group, mapAccumL, foldl')
import Data.List.Index (setAt, indexed, deleteAt, ifoldl', ifind)
import Data.List.Split (splitOn)
import Data.Set (intersection, fromList, toList)
import Data.Tree (Tree(..))

import Debug.Trace

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
    "d5p1" -> d5p1
    "d5p2" -> d5p2
    "d6p1" -> d6p1

makeAdjacencyList :: String -> (String, String, [String])
makeAdjacencyList s =
  let [p, c] = splitOn ")" s
  in (p, p, [c])

fixDups
  :: [(String, String, [String])]
  -> [(String, String, [String])]
  -> [(String, String, [String])]
fixDups ys [] = ys
fixDups ys ((n, k, as):xs) =
  let (is, adjs) =
        unzip $ ifoldl'
        (\acc i (m, _, adj) -> if n == m then (i, adj!!0):acc else acc)
        [] xs
      xs' = ifoldl'
        (\acc i e -> if elem i is then acc else e:acc)
        [] xs
  in fixDups ((n, k, as ++ adjs) : ys) xs'

fixLeaves
  :: [(String, String, [String])]
  -> [(String, String, [String])]
  -> [(String, String, [String])]
  -> [(String, String, [String])]
fixLeaves acc [] _ = acc
fixLeaves acc (p@(n, k, as):xs) ys =
  let fst (a,_,_) = a
      as' = foldl' (\acc a -> if elem a (map fst ys) then acc else a:acc)
            [] as
  in fixLeaves (p : map (\a -> (a, a, [])) as' ++ acc) xs ys

fixAdjacencies
  :: [(String, String, [String])]
  -> [(String, String, [String])]
  -> [(String, String, [String])]
fixAdjacencies acc [] = acc
fixAdjacencies acc (p@(n, k, as):xs) =
  let
    fst' (a,_,_) = a; snd' (_,b,_) = b; thd (_,_,c) = c
    f a = let (i, x) = fromJust $ ifind (\_ s -> fst' s == a) acc
              u = (fst' x, snd' x, n : thd x)
          in (i, u)
    (is, ys) = unzip $ map f as
    acc' = fst $ ifoldl'
           (\(zs, j) i a -> if i `elem` is
                            then (setAt i (ys!!j) zs, j+1)
                            else (zs, j)) (acc, 0) acc
  in fixAdjacencies acc' xs


d6p2 :: IO ()
d6p2 = do
  input <- lines <$> readFile "input6"
  let is = fixDups [] $ map makeAdjacencyList input
      is' = fixLeaves [] is is
      is'' = fixAdjacencies is' is'
      (g, nfv, vfk) = graphFromEdges is''
      t = head $ dfs g [fromJust $ vfk "YOU"]
      san = fromJust $ vfk "SAN"
      findSanta :: [(Int, Int)] -> Tree Int -> [(Int, Int)]
      findSanta acc (Node a l) =
        if elem san $ map rootLabel l
        then acc
        else concatMap (\b -> findSanta ((a, rootLabel b) : acc) b) l
  putStrLn $ show $ (length $ findSanta [] t) - 1

d6p1 :: IO ()
d6p1 = do
  input <- lines <$> readFile "input6"
  let is = fixDups [] $ map makeAdjacencyList input
      (g, fn, fv) =
        graphFromEdges
        $ fixLeaves [] is is
      sumOrbits :: Int -> Vertex -> Int
      sumOrbits d v =
        let (_,_,as) = fn v
            adjs = map (sumOrbits (d+1)) $ mapMaybe fv as
        in d + sum adjs
  putStrLn $ show $ sumOrbits 0 $ fromJust $ fv "COM"

d5p2 :: IO ()
d5p2 = do
  rawInput <- readFile "input5"
  let input = map read $ splitOn "," rawInput :: [Int]
  computed <- compute' 0 $ return input
  putStrLn ""

d5p1 :: IO ()
d5p1 = do
  rawInput <- readFile "input5"
  let input = map read $ splitOn "," rawInput :: [Int]
  computed <- compute' 0 $ return input
  putStrLn ""

compute' :: Int -> IO [Int] -> IO [Int]
compute' i iocode = do
  code <- iocode
  --putStrLn $ "Index: " ++ show i
  --putStrLn $ "Code: " ++ (show $ take 10 $ drop i code)
  let ds = fillOp $ code!!i
      getValue m p = if m == 0 then code!!p else p
      doOp b v1 v2 v3 = return $ setAt v3 (v1 `b` v2) code
      doInput i' = do
        putStrLn "Provide input:"
        v <- read <$> getLine :: IO Int
        return $ setAt i' v code
      doOutput v = do
        putStrLn $ "Output: " ++ show v
        return code
  case ds of
    (_:_:_:9:9:[]) -> return code
    (_:m2:m1:0:1:[]) -> compute' (i+4)
                        $ doOp (+) (getValue m1 $ code!!(i+1))
                        (getValue m2 $ code!!(i+2))
                        (code!!(i+3))
    (_:m2:m1:0:2:[]) -> compute' (i+4)
                        $ doOp (*) (getValue m1 $ code!!(i+1))
                        (getValue m2 $ code!!(i+2))
                        (code!!(i+3))
    (_:_:m1:0:3:[]) -> compute' (i+2) $ doInput $ code!!(i+1)
    (_:_:m1:0:4:[]) -> compute' (i+2) $ doOutput $ getValue m1 $ code!!(i+1)
    (_:m2:m1:0:5:[]) ->
      let b = (getValue m1 $ code!!(i+1)) /= 0
      in compute' (if b then getValue m2 $ code!!(i+2) else i+3)
         $ return code
    (_:m2:m1:0:6:[]) ->
      let b = (getValue m1 $ code!!(i+1)) == 0
      in compute' (if b then getValue m2 $ code!!(i+2) else i+3)
         $ return code
    (_:m2:m1:0:7:[]) ->
      let b = (getValue m1 $ code!!(i+1)) < (getValue m2 $ code!!(i+2))
      in compute' (i+4) $
         return $ setAt (code!!(i+3)) (if b then 1 else 0) code
    (_:m2:m1:0:8:[]) ->
      let b = (getValue m1 $ code!!(i+1)) == (getValue m2 $ code!!(i+2))
      in compute' (i+4) $
         return $ setAt (code!!(i+3)) (if b then 1 else 0) code

fillOp :: Int -> [Int]
fillOp x =
  let pad y = if length y < 5 then pad (0:y) else y
  in pad $ digits 10 x

d4p2 :: IO ()
d4p2 = do
  let ns = [152085..670283]
      containsPair n =
        let ns = digits 10 n
        in not . null . filter (\x -> length x == 2) $ group ns
      pws = filter containsPair $ filter isAscending ns
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
