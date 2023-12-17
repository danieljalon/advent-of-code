import Data.Array
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)

import Debug.Trace (trace)  -- TODO: Delete

type Loc = (Int, Int)

getCharsInFile :: String -> [(Loc, Char)]
getCharsInFile [] = []
getCharsInFile file = parseFile file (1, 1) []
    where
        parseFile file (x, y) chars 
            | null file = chars
            | head file == '\n' = parseFile (tail file) (1, y + 1) chars
            | otherwise =
                parseFile (tail file) (x + 1, y) (chars ++ [char])
            where
                char = ((x, y), head file) 

getNextDigit :: [(Loc, Char)] -> (Maybe (Loc, Loc, Int), [(Loc, Char)]) 
getNextDigit [] = (Nothing, []) 
getNextDigit chars =
    if isDigit ch 
        then go (tail chars) loc loc [ch]
        else getNextDigit (tail chars) 
    where
        (loc, ch) = head chars
        go [] startLoc atLoc accum = (Just (startLoc, atLoc, read accum), [])
        go chars startLoc atLoc accum = 
            if isDigit ch
                then go (tail chars) startLoc loc (accum ++ [ch]) 
                else (Just (startLoc, atLoc, read accum), tail chars) 
            where
                (loc, ch) = head chars

getDigits :: [(Loc, Char)] -> [(Loc, Loc, Int)]
getDigits chars = go chars []
    where
        go chars accum =
            if isJust locDigit
                then go remChars (accum ++ [fromJust locDigit]) 
                else accum
            where
                (locDigit, remChars) = getNextDigit chars

isSymbol :: Char -> Bool
isSymbol ch
    | ch == '.' = False
    | isDigit ch = False
    | otherwise = True

withinBound :: Array (Int, Int) Char -> Loc -> Bool
withinBound schematic loc
    | x < xIni || x > xEnd = False
    | y < yIni || y > yEnd = False
    | otherwise = True
    where
        (x, y) = loc
        ((xIni, yIni), (xEnd, yEnd)) = bounds schematic

isPartNumber :: Loc -> Loc -> Array (Int, Int) Char -> Bool
isPartNumber startLoc endLoc schematic =
    any isSymbol searchChars

    where
        (xIni, yIni) = startLoc
        (xEnd, yEnd) = endLoc
        searchLoc = [loc | x <- [xIni - 1 .. xEnd + 1],
                           let loc = (x, yIni - 1),
                           withinBound schematic loc] ++
                    [loc | x <- [xIni - 1 .. xEnd + 1],
                           let loc = (x, yIni + 1),
                           withinBound schematic loc] ++
                    [loc | x <- [xIni - 1, xEnd + 1],
                           let loc = (x, yIni),
                           withinBound schematic loc]
        searchChars = [schematic ! loc | loc <- searchLoc] 

getNumberFromLoc :: Loc -> Array (Int, Int) Char -> Int 
getNumberFromLoc loc schematic =
    read (getLeftDigits loc schematic (getRightDigits loc schematic [schematic ! loc]))
    where
        getLeftDigits (x, y) schematic accum
            | not (withinBound schematic newLoc) = accum
            | not (isDigit ch) = accum
            | otherwise =
                 getLeftDigits newLoc schematic ([ch] ++ accum)
            where
                newLoc = (x - 1, y)
                ch = schematic ! newLoc
            
        getRightDigits (x, y) schematic accum
            | not (withinBound schematic newLoc) = accum
            | not (isDigit ch) = accum
            | otherwise =
                getRightDigits newLoc schematic (accum ++ [ch])
            where
                newLoc = (x + 1, y)
                ch = schematic ! newLoc

getSurroundingDigits :: Loc -> Array (Int, Int) Char -> [Loc]
getSurroundingDigits loc schematic =
    upperLoc ++ 
    (if numAt4 then [loc4] else []) ++
    (if numAt5 then [loc5] else []) ++
    lowerLoc

    where
        numAt loc schematic =
            if withinBound schematic loc
                then isDigit (schematic ! loc)
                else False
        (x, y) = loc

        loc1 = (x - 1, y - 1)
        numAt1 = numAt loc1 schematic
        loc2 = (x, y - 1)
        numAt2 = numAt loc2 schematic
        loc3 = (x + 1, y - 1)
        numAt3 = numAt loc3 schematic
        loc4 = (x - 1, y)
        numAt4 = numAt loc4 schematic
        loc5 = (x + 1, y)
        numAt5 = numAt loc5 schematic
        loc6 = (x - 1, y + 1)
        numAt6 = numAt loc6 schematic
        loc7 = (x, y + 1)
        numAt7 = numAt loc7 schematic
        loc8 = (x + 1, y + 1)
        numAt8 = numAt loc8 schematic

        upperLoc
            | numAt1 && numAt3 && (not numAt2) = [loc1, loc3]
            | numAt1 = [loc1]
            | numAt2 = [loc2]
            | numAt3 = [loc3]
            | otherwise = []

        lowerLoc
            | numAt6 && numAt8 && (not numAt7) = [loc6, loc8]
            | numAt6 = [loc6]
            | numAt7 = [loc7]
            | numAt8 = [loc8]
            | otherwise = []
    
calcGearRatio :: Loc -> Array (Int, Int) Char -> Maybe Int
calcGearRatio astLoc schematic =
    if length totalLocs == 2
        then Just (num1 * num2) 
        else Nothing
    where
        totalLocs = getSurroundingDigits astLoc schematic
        num1 = getNumberFromLoc (totalLocs !! 0) schematic
        num2 = getNumberFromLoc (totalLocs !! 1) schematic

assert :: Bool -> String -> String -> IO ()
assert cond okMsg errMsg =
    if cond
        then putStrLn okMsg
        else putStrLn errMsg


getSquare :: Loc -> Array (Int, Int) Char -> String
getSquare (x, y) schematic = insertNewLine str 1 []
    where
        str = [schematic ! loc | j <- [-1..1], i <- [-1..1], let loc = (x+i,y+j), withinBound schematic loc]

        insertNewLine :: String -> Int -> String -> String
        insertNewLine [] index accum = accum ++ ['\n']
        insertNewLine str index accum =
            if index == 3
                then insertNewLine (tail str) 1 (accum ++ [head str] ++ ['\n'])
                else insertNewLine (tail str) (index + 1) (accum ++ [head str])
                    

test = do
    contents <- readFile "2023d3.input"
    let chars = getCharsInFile contents

    let (ini, _) = head chars
    let (end, _) = last chars
    let schematic = array (ini, end) chars

    let almostGears = [loc | loc <- indices schematic,
                             let ch = schematic ! loc,
                             ch == '*',
                             let gr = calcGearRatio loc schematic,
                             not (isJust gr)]
    putStrLn (show (getSurroundingDigits (5, 66) schematic))
    --mapM_ putStrLn ([ (show loc) ++ ['\n'] ++ (getSquare loc schematic) | loc <- almostGears] ) 



main = do
    contents <- readFile "2023d3.input"
    let chars = getCharsInFile contents

    let (ini, _) = head chars
    let (end, _) = last chars
    let schematic = array (ini, end) chars
    let partNumbers = [n | (startLoc, endLoc, n) <- getDigits chars,
                           isPartNumber startLoc endLoc schematic]

    let gearRatios = [fromJust gr | loc <- indices schematic,
                                    let ch = schematic ! loc,
                                    ch == '*',
                                    let gr = calcGearRatio loc schematic,
                                    isJust gr]

    putStrLn("The sum of all part numbers is: " ++ show (sum partNumbers))
    putStrLn("The sum of all gear ratios is: " ++ show (sum gearRatios))

