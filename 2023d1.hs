import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)

main :: IO ()

-- Part 1
getFirstDigit :: String -> Char
getFirstDigit xs = if isDigit x then x else getFirstDigit (tail xs)
    where
        x = head xs

getNumberInLine :: String -> Int
getNumberInLine xs = read ([getFirstDigit xs] ++ [getFirstDigit (reverse xs)]) 

-- Part 2
spelledDigit2Int :: [(String, Int)]
spelledDigit2Int = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)]

spelledDigitRev2Int :: [(String, Int)]
spelledDigitRev2Int = map (\(x, y) -> (reverse x, y)) spelledDigit2Int

startsWithSpelledDigit :: String -> [(String, Int)] -> Maybe Int
startsWithSpelledDigit xs digitMap
    | xs == [] = Nothing
    | digitMap == [] = Nothing
    | isPrefixOf prefix xs = Just num 
    | otherwise = startsWithSpelledDigit xs (tail digitMap)
    where
        (prefix, num) = head digitMap

lStartsWithSpelledDigit :: String -> Maybe Int
lStartsWithSpelledDigit xs = startsWithSpelledDigit xs spelledDigit2Int

rStartsWithSpelledDigit :: String -> Maybe Int
rStartsWithSpelledDigit xs = startsWithSpelledDigit xs spelledDigitRev2Int

getDigitInLine :: String -> (String -> Maybe Int) -> Int
getDigitInLine xs startsWithSpelledDigit
    | isDigit x = read [x]
    | isJust y = fromJust y
    | otherwise = getDigitInLine (tail xs) startsWithSpelledDigit
    where
        x = head xs
        y = startsWithSpelledDigit xs

getNumberInLinePt2 :: String -> Int
getNumberInLinePt2 xs =
    10 * getDigitInLine xs lStartsWithSpelledDigit +
    getDigitInLine (reverse xs) rStartsWithSpelledDigit

main = do
    contents <- readFile "2023d1.input"
    let linesOfFile = lines contents
    putStrLn ("Sum of all calibration values (pt1): " ++ show (sum (map getNumberInLine linesOfFile))) 
    putStrLn ("Sum of all calibration values (pt2): " ++ show (sum (map getNumberInLinePt2 linesOfFile)))

