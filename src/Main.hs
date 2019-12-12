module Main where

import Data.Char (digitToInt)
import Data.Digits (digits)
import Data.Graph (dfs, graphFromEdges, Vertex)
import Data.List (elemIndex, genericIndex, group, mapAccumL, foldl'
                 , permutations, transpose)
import Data.List.Index (setAt, ifoldl', ifind)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (intersection, fromList, toList)
import Data.Tree (Tree(..))
import System.Environment (getArgs)

--import Debug.Trace

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
    "d7p1" -> d7p1
    "d7p2" -> d7p2
    "d8p1" -> d8p1
    "d8p2" -> d8p2
    "d9p1" -> d9p1
    "d9p2" -> d9p2
    "d10p1" -> d10p1
    "d10p2" -> d10p2
    _ -> error "bad arg"

d10p2 :: IO ()
d10p2 = do
  undefined

data Point a = Point a a deriving (Eq, Show)

angle :: (RealFrac a, RealFloat a) => Point a -> Point a -> a
angle (Point x y) (Point x' y') = atan2 (y - y') (x - x')

isVisible :: (RealFrac a, RealFloat a) => Point a -> Point a -> [a] -> Bool
isVisible p p' xs
  | p == p' = False
  | otherwise = not $ angle p p' `elem` xs

numVisible :: (RealFrac a, RealFloat a)
  => [a] -> Point a -> [[Space]] -> [Point a] -> Int
numVisible acc _ _ [] = length acc
numVisible acc m sp (n@(Point x y):ns) =
  if sp!!round y!!round x == Asteroid && isVisible m n acc
  then numVisible (angle m n : acc) m sp ns
  else numVisible acc m sp ns

data Space = Asteroid | Space deriving (Show, Eq)

space :: Char -> Space
space '#' = Asteroid
space '.' = Space
space _ = Space

d10p1 :: IO ()
d10p1 = do
  m <- map (map space) <$> (init . splitOn "\n") <$> readFile "input10"
  let ns = map (\(a,b) -> Point (fromIntegral a) (fromIntegral b))
           $ (,) <$> [0..length m - 1] <*> [0..length (head m) - 1]
      allNums = map (\p@(Point x y) ->
                 if m!!round y!!round x == Asteroid
                 then numVisible [] p m ns else -1) ns
  putStrLn $ show $ maximum allNums

d9p2 :: IO ()
d9p2 = do
  is <- map read <$> splitOn "," <$> readFile "input9"
  let c = compute9 0 0 [2] [] (is ++ repeat 0)
  putStrLn $ show c

d9p1 :: IO ()
d9p1 = do
  is <- map read <$> splitOn "," <$> readFile "input9"
  let c = compute9 0 0 [1] [] (is ++ repeat 0)
  putStrLn $ show c

(!^) :: [a] -> Integer -> a
(!^) = genericIndex
infixl 9 !^

compute9
  :: Integer
  -> Integer
  -> [Integer]
  -> [Integer]
  -> [Integer]
  -> [Integer]
compute9 i r input output code =
  let ds = fillOp $ code!^i
      getv m p = case m of
        0 -> code!^p
        1 -> p
        2 -> code!^(r+p)
        _ -> error "bad mode"
      getv' m p = case m of
        0 -> p
        2 -> r+p
        _ -> error "bad mode"
      doOp b v1 v2 v3 = setAt (fromInteger v3) (v1 `b` v2) code
  in case ds of
    (_:_:_:9:9:[]) -> reverse output
    (m3:m2:m1:0:1:[]) ->
      compute9 (i+4) r input output $ doOp (+) (getv m1 $ code!^(i+1))
      (getv m2 $ code!^(i+2)) (getv' m3 $ code!^(i+3))
    (m3:m2:m1:0:2:[]) ->
      compute9 (i+4) r input output $ doOp (*) (getv m1 $ code!^(i+1))
      (getv m2 $ code!^(i+2)) (getv' m3 $ code!^(i+3))
    (_:_:m1:0:3:[]) ->
      let doInput i' = setAt (fromInteger i') (head input) code
      in compute9 (i+2) r (tail input) output
         $ doInput $ getv' m1 $ code!^(i+1)
    (_:_:m1:0:4:[]) ->
      compute9 (i+2) r input ((getv m1 $ code!^(i+1)) : output) code
    (_:m2:m1:0:5:[]) ->
      let b = (getv m1 $ code!^(i+1)) /= 0
      in compute9
         (if b then getv m2 $ code!^(i+2) else i+3) r input output code
    (_:m2:m1:0:6:[]) ->
      let b = (getv m1 $ code!^(i+1)) == 0
      in compute9
         (if b then getv m2 $ code!^(i+2) else i+3) r input output code
    (m3:m2:m1:0:7:[]) ->
      let b = (getv m1 $ code!^(i+1)) < (getv m2 $ code!^(i+2))
      in compute9 (i+4) r input output
         $ setAt (fromInteger $ getv' m3 $ code!^(i+3)) (if b then 1 else 0) code
    (m3:m2:m1:0:8:[]) ->
      let b = (getv m1 $ code!^(i+1)) == (getv m2 $ code!^(i+2))
      in compute9 (i+4) r input output
         $ setAt (fromInteger $ getv' m3 $ code!^(i+3)) (if b then 1 else 0) code
    (_:_:m1:0:9:[]) ->
      compute9 (i+2) (r+getv m1 (code!^(i+1))) input output code
    _ -> error "bad code"

d8p2 :: IO ()
d8p2 = do
  is <- map digitToInt <$> readFile "input8"
  let (w,h) = (25,6)
      is' = transpose $ chunksOf (w*h) is
      getPixel [] = error "reached end with no pixel"
      getPixel (x:xs) = if x == 0 || x == 1 then x else getPixel xs
      im = chunksOf 25 $ map ((\x -> if x == 1 then 'X' else ' ') . getPixel) is'
  mapM_ putStrLn im

d8p1 :: IO ()
d8p1 = do
  is <- map digitToInt <$> readFile "input8"
  let (w, h) = (25,6)
      is' = chunksOf (w*h) is
      count n = length . filter ((==) n)
      is'' = map (\xs -> (count 0 xs, xs)) is'
      least0s =
        snd $ foldl' (\acc@(n, _) (m, xs) -> if m < n then (m, xs) else acc)
        (head is'') (tail is'')
      out = count 1 least0s * count 2 least0s
  putStrLn $ show out

d7p2 :: IO ()
d7p2 = do
  is <- map read <$> splitOn "," <$> readFile "input7"
  let ampA i j k l m = compute'' 0 (i:0:ampE i j k l m) [] is
      ampD i j k l m = compute'' 0 (l:ampC i j k l m) [] is
      ampC i j k l m = compute'' 0 (k:ampB i j k l m) [] is
      ampB i j k l m = compute'' 0 (j:ampA i j k l m) [] is
      ampE i j k l m = compute'' 0 (m:ampD i j k l m) [] is
  putStrLn . show . maximum $ map (\(a:b:c:d:e:[]) -> ampE a b c d e)
    $ permutations [5..9]

d7p1 :: IO ()
d7p1 = do
  is <- map read <$> splitOn "," <$> readFile "input7"
  let ampA i = compute'' 0 (i:repeat 0) [] is
      ampB i j = compute'' 0 (i:ampA j) [] is
      ampC i j k = compute'' 0 (i:ampB j k) [] is
      ampD i j k l = compute'' 0 (i:ampC j k l) [] is
      ampE (i, j, k, l, m) = compute'' 0 (i:ampD j k l m) [] is
  putStrLn . show . maximum $ map (head . ampE)
    $ map (\[a,b,c,d,e] -> (a,b,c,d,e)) $ permutations [0..4]

d5p1' :: IO ()
d5p1' = do
  is <- map read <$> splitOn "," <$> readFile "input5"
  let computed = compute'' 0 [1] [] is
  putStrLn $ show computed

d5p2' :: IO ()
d5p2' = do
  is <- map read <$> splitOn "," <$> readFile "input5"
  let computed = compute'' 0 [5] [] is
  putStrLn $ show computed

compute'' :: Int -> [Int] -> [Int] -> [Int] -> [Int]
compute'' i input output code =
  let ds = fillOp $ code!!i
      getv m p = if m == 0 then code!!p else p
      doOp b v1 v2 v3 = setAt v3 (v1 `b` v2) code
      doInput i' = setAt i' (head input) code
  in case ds of
    (_:_:_:9:9:[]) -> reverse output
    (_:m2:m1:0:1:[]) -> compute'' (i+4) input output
                        $ doOp (+) (getv m1 $ code!!(i+1))
                        (getv m2 $ code!!(i+2))
                        (code!!(i+3))
    (_:m2:m1:0:2:[]) -> compute'' (i+4) input output
                        $ doOp (*) (getv m1 $ code!!(i+1))
                        (getv m2 $ code!!(i+2))
                        (code!!(i+3))
    (_:_:_:0:3:[]) ->
      compute'' (i+2) (tail input) output $ doInput $ code!!(i+1)
    (_:_:m1:0:4:[]) ->
      compute'' (i+2) input ((getv m1 $ code!!(i+1)) : output) code
    (_:m2:m1:0:5:[]) ->
      let b = (getv m1 $ code!!(i+1)) /= 0
      in compute''
         (if b then getv m2 $ code!!(i+2) else i+3) input output code
    (_:m2:m1:0:6:[]) ->
      let b = (getv m1 $ code!!(i+1)) == 0
      in compute''
         (if b then getv m2 $ code!!(i+2) else i+3) input output code
    (_:m2:m1:0:7:[]) ->
      let b = (getv m1 $ code!!(i+1)) < (getv m2 $ code!!(i+2))
      in compute'' (i+4) input output
         $ setAt (code!!(i+3)) (if b then 1 else 0) code
    (_:m2:m1:0:8:[]) ->
      let b = (getv m1 $ code!!(i+1)) == (getv m2 $ code!!(i+2))
      in compute'' (i+4) input output
         $ setAt (code!!(i+3)) (if b then 1 else 0) code
    _ -> error "bad code"

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
fixLeaves acc (p@(_,_,as):xs) ys =
  let fst' (a,_,_) = a
      as' = foldl' (\acc' a ->
                      if elem a (map fst' ys) then acc' else a:acc')
            [] as
  in fixLeaves (p : map (\a -> (a, a, [])) as' ++ acc) xs ys

fixAdjacencies
  :: [(String, String, [String])]
  -> [(String, String, [String])]
  -> [(String, String, [String])]
fixAdjacencies acc [] = acc
fixAdjacencies acc ((n,_,as):xs) =
  let
    fst' (a,_,_) = a; snd' (_,b,_) = b; thd (_,_,c) = c
    f a = let (i, x) = fromJust $ ifind (\_ s -> fst' s == a) acc
              u = (fst' x, snd' x, n : thd x)
          in (i, u)
    (is, ys) = unzip $ map f as
    acc' = fst $ ifoldl'
           (\(zs, j) i _ -> if i `elem` is
                            then (setAt i (ys!!j) zs, j+1)
                            else (zs, j)) (acc, 0) acc
  in fixAdjacencies acc' xs

d6p2 :: IO ()
d6p2 = do
  input <- lines <$> readFile "input6"
  let is = fixDups [] $ map makeAdjacencyList input
      is' = fixLeaves [] is is
      is'' = fixAdjacencies is' is'
      (g, _, vfk) = graphFromEdges is''
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
      (_, fn, fv) =
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
  _ <- compute' 0 $ return input
  putStrLn ""

d5p1 :: IO ()
d5p1 = do
  rawInput <- readFile "input5"
  let input = map read $ splitOn "," rawInput :: [Int]
  _ <- compute' 0 $ return input
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
        v <- read <$> getLine
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
    (_:_:_:0:3:[]) ->
      compute' (i+2) $ doInput $ code!!(i+1)
    (_:_:m1:0:4:[]) ->
      compute' (i+2) $ doOutput $ getValue m1 $ code!!(i+1)
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
    _ -> error "bad code"

fillOp :: Integral a => a -> [a]
fillOp x =
  let pad y = if length y < 5 then pad (0:y) else y
  in pad $ digits 10 x

d4p2 :: IO ()
d4p2 = do
  let ns = [152085..670283]
      containsPair n =
        let ns' = digits 10 n
        in not . null . filter (\x -> length x == 2) $ group ns'
      pws = filter containsPair $ filter isAscending ns
  putStrLn . show $ length pws

isAscending :: Int -> Bool
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
  let doMove _ [] = error ""
      doMove (x,y) (d:m) =
        let n = read m :: Integer
        in case d of
          'R' -> [(x',y) | x' <- [(x+1)..(x+n)]]
          'L' -> reverse [(x',y) | x' <- [(x-n)..(x-1)]]
          'U' -> [(x,y') | y' <- [(y+1)..(y+n)]]
          'D' -> reverse [(x,y') | y' <- [(y-n)..(y-1)]]
          _ -> error "bad move"
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
isNums [] _ _ = False
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
    _ -> error "bad code"
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
