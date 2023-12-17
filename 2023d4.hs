import Data.Set (Set, fromList, member)

split :: Char -> String -> [String]
split ch str = go ch str [] []
    where
        go ch [] accum out = if null accum then out else out ++ [accum]
        go ch str accum out
            | (head str) /= ch = go ch (tail str) (accum ++ [head str]) out
            | null accum = go ch (tail str) accum out  
            | otherwise = go ch (tail str) [] (out ++ [accum])

parseLine :: String -> ([Int], [Int])
parseLine line = (winNums, nums)
    where
        (_:str:_) = split ':' line
        (str1:str2:_) = split '|' str
        winNums = map read (split ' ' str1)
        nums = map read (split ' ' str2)

checkNum :: Set Int -> Int -> Int -> Int
checkNum winNumsSet num accum =
    if member num winNumsSet
        then accum + 1
        else accum

calcPoints :: [Int] -> [Int] -> Int
calcPoints winNums nums = if prized > 0 then 2 ^ (prized - 1) else 0
    where
        winNumsSet = fromList winNums
        prized = foldr (checkNum winNumsSet) 0 nums

sumLists :: [Int] -> [Int] -> [Int]
sumLists l1 l2 = go l1 l2 []
    where
        go [] l2 accum = accum ++ l2
        go l1 [] accum = accum ++ l1
        go l1 l2 accum = go (tail l1) (tail l2) (accum ++ [head l1 + head l2])

checkCard :: [Int] -> [Int] -> [Int] -> (Int, [Int])
checkCard winNums nums multipliers = ( 1 + m, sumLists mx prized)
    where
        (m:mx) = if null multipliers then [0] else multipliers
        winNumsSet = fromList winNums
        prized = [(1 + m) | n <- nums, member n winNumsSet]

checkCards :: [String] -> [Int]
checkCards lines = go lines [] []
    where
        go [] multipliers accum = accum
        go lines multipliers accum = go (tail lines) mults (accum ++ [cardNum])
            where
                (winNums, nums) = parseLine (head lines)
                (cardNum, mults) = checkCard winNums nums multipliers



main = do
    contents <- readFile "2023d4.input"
    let linesOfFile = lines contents

    -- Part 1
    let scores = [calcPoints winNums nums | line <- linesOfFile,
                                            let (winNums, nums) = parseLine line]
    putStrLn ("The scratch cards are worth " ++ (show (sum scores)) ++ " points.")

    --Part 2
    let cardsNum = checkCards linesOfFile
    putStrLn ("We ended up with " ++ (show (sum cardsNum)) ++ " scratchcards.")

