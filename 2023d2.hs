import Data.Char (isDigit)

data CubeCount = CubeCount
    { red :: Int
    , blue :: Int
    , green :: Int } deriving (Show)

feedStrFromStrUntil :: String -> String -> (String -> Bool) -> (String, String)
feedStrFromStrUntil str1 [] f = (str1, []) 
feedStrFromStrUntil str1 str2 f =
    if f str2
        then (str1, str2)
        else feedStrFromStrUntil (str1 ++ [head str2]) (tail str2) f

charIn :: Char -> String -> Bool
charIn ch [] = False
charIn ch str = if ch == head str then True else charIn ch (tail str)

getToken :: String -> String -> (String, String)
getToken [] delimiters = ([], [])
getToken str delimiters
    | charIn (head str) delimiters = getToken (tail str) delimiters
    | null str2 = (str1, str2)
    | otherwise = (str1, tail str2)
    where
        (str1, str2) = feedStrFromStrUntil [] str (\xs -> charIn (head xs) delimiters)

-- Delimiters used in Game Description
delimiters = " ,;"

tokenize :: String -> [String]
tokenize str = go str [] 
    where
        go [] acc = acc
        go s acc =
            if null token
                then acc
                else go remStr (acc ++ [token])
            where
                (token, remStr) = getToken s delimiters

updateCubeCount :: CubeCount -> Int -> String -> CubeCount
updateCubeCount cnt num color
    | color == "red" = if red cnt < num then cnt {red = num} else cnt
    | color == "blue" = if blue cnt < num then cnt {blue = num} else cnt
    | color == "green" = if green cnt < num then cnt {green = num} else cnt

parseGame :: String -> CubeCount
parseGame str = go tokens (CubeCount {red = 0, blue = 0, green = 0})
    where
        tokens = tokenize str
        go [] acc = acc
        go (num:color:remTokens) acc = go remTokens (updateCubeCount acc (read num) color) 

split :: Char -> String -> (String, String)
split ch [] = ([], [])
split ch str =
    (str1, tail str2)
    where
        (str1, str2) = feedStrFromStrUntil [] str (\(x:xs) -> ch == x)

parseLine :: String -> (Int, CubeCount)
parseLine line = (id, game) 
    where
        (sId, sGame) = split ':' line
        id = read (dropWhile (\x -> not (isDigit x)) sId) 
        game = parseGame sGame
 
gamePossible :: CubeCount -> Bool
gamePossible count =
    if red count > 12 || green count > 13 || blue count > 14
        then False else True

main = do
    contents <- readFile "2023d2.input"
    let linesOfFile = lines contents
    let idSum = sum [id | (id, count) <- (map parseLine linesOfFile), gamePossible count]
    let powerSum = sum [red count * green count * blue count |
                        (_, count) <- (map parseLine linesOfFile)]
    putStrLn ("The sum of IDs of the possible games is " ++ show idSum) 
    putStrLn ("The sum of the power of every game is " ++ show powerSum) 

