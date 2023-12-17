import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)

import Debug.Trace  -- TODO: Delete

--
-- Utilities
--

isNumber :: String -> Bool
isNumber [] = False
isNumber (c:cs) =
    if isDigit c 
        then (if null cs then True else isNumber cs)
        else False

takeWhileFrom :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileFrom while [] = ([], [])
takeWhileFrom while from =
    if while x
        then ([x] ++ took, notTook)
        else ([], from)
    where
        (x:xs) = from
        (took, notTook) = takeWhileFrom while xs

groupBy2s :: [Int] -> [(Int, Int)]
groupBy2s [] = []
groupBy2s (x1:x2:xs) = [(x1, x2)] ++ groupBy2s xs

groupBy3s :: [Int] -> [(Int, Int, Int)]
groupBy3s [] = []
groupBy3s (x1:x2:x3:xs) = [(x1, x2, x3)] ++ groupBy3s xs

minIn :: [Int] -> Int
minIn list = go (tail list) (head list)
    where
        go [] currMin = currMin
        go list currMin = go (tail list) (min currMin (head list))

--
-- Parsing
--

matches :: Char -> String -> Bool
matches ch [] = False
matches ch chars = if ch == c then True else matches ch cs
    where
        (c: cs) = chars

separators = " :\n"

tokenize :: String -> [String]
tokenize contents = go contents [] []
    where
        go contents tokens token
            | null contents = t
            | matches c separators = go cs t []
            | otherwise = go cs tokens (token ++ [c])
            where
                (c:cs) = contents
                t = if null token then tokens else (tokens ++ [token])

parseMaps :: [String] -> [[(Int, Int, Int)]]
parseMaps tokens
    | null tokens = []
    | null took = parseMaps (tail tokens)
    | otherwise = [groupBy3s (map read took)] ++ (parseMaps notTook)
    where
        (took, notTook) = takeWhileFrom isNumber tokens

parseFile :: String -> ([Int], [[(Int, Int, Int)]])
parseFile contents = ([read x | x <- seeds], maps)
    where
        (_:tokens) = tokenize contents
        (seeds, remTokens) = takeWhileFrom isNumber tokens
        maps = map (sortBy (\(_, x, _) (_, y, _) -> compare x y)) (parseMaps remTokens) 

--
-- Problem
--

inRange :: Int -> Int -> Int -> Int -> Int
inRange destStart sourStart range num =
    if sourStart <= num && num < (sourStart + range)
        then destStart + num - sourStart
        else num

findClosest :: Int -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
findClosest num [] = Nothing
findClosest num map =
    if num >= ss
        then (if isJust c then c else Just (head map))
        else Nothing
    where
        (_, ss, _) = head map
        c = findClosest num (tail map)

-- findClosest :: Int -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
-- findClosest num [] = Nothing
-- findClosest num map = go num map Nothing
--     where
--         go num [] accum = accum
--         go num map accum = 
--             if num >= ss
--                 then go num (tail map) (Just (head map))
--                 else accum
--             where
--                 (_, ss, _) = head map
            


applyMap :: Int -> [(Int, Int, Int)] -> Int
applyMap num [] = num
applyMap num map =
    if isJust c 
        then inRange ds ss r num
        else num
    where
        c = findClosest num map
        (ds, ss, r) = fromJust (c)

applyMaps :: [[(Int, Int, Int)]] -> Int -> Int
applyMaps maps seed = foldl applyMap seed maps


getMinInRange :: Int -> Int -> [[(Int, Int, Int)]] -> Int
getMinInRange source range maps =
    if range == 1
        then loc
        else min loc (getMinInRange (source + 1) (range - 1) maps)
    where
        loc = applyMaps maps source


main = do
    contents <- readFile "2023d5.input"

    -- Part 1
    let (seeds, maps) = parseFile contents
    let locations = [applyMaps maps s | s <- seeds]
    putStrLn ("The lowest location number is: " ++ show (minIn locations)) 

    -- Part 2
    let seeds2 = groupBy2s seeds
    putStrLn ("PT2 " ++ show (seeds2)) 
    let (a, b) = head seeds2
    putStrLn ("PT2 " ++ show (getMinInRange 950527520 (85181200 `div` 100)  maps))

    --let locPt2 = minIn [getMinInRange s r maps | (s, r) <- groupBy2s seeds]
    --putStrLn ("The lowest location number is (Pt2): " ++ show (locPt2)) 

