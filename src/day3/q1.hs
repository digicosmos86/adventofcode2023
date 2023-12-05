import Text.Regex.Posix
import Data.Char (isDigit)
import Data.Map (Map, fromList, member)
import qualified Data.Map as Map

data Part = Part {
    number :: Int,
    row :: Int,
    indices :: (Int, Int)
} deriving (Show)

parsePart :: Int -> String -> [Part]
parsePart row str = let
    numbers = getAllTextMatches (str =~ "[0-9]+") :: [String]
    indices = getAllMatches (str =~ "[0-9]+") :: [(Int, Int)]
    in zipWith (\num (start, length) -> Part {
        number = read num :: Int,
        row = row,
        indices = (start, start + length)
    }) numbers indices

parseInput :: [String] -> [Part]
parseInput inputs = concat $ zipWith parsePart [0..] inputs

parseInputToMap :: [String] -> Map Int [Part]
parseInputToMap inputs = fromList $ zip [0..] $ zipWith parsePart [0..] inputs

isASymbol :: Char -> Bool
isASymbol c = c /= '.' && not (isDigit c)

symbolIndices :: String -> [Int]
symbolIndices str = [i | (i, c) <- zip [0..] str, isASymbol c]

allStars :: [String] -> [(Int, Int)]
allStars inputs = concat $ zipWith allPoundsLine [0..] inputs
    where allPoundsLine row line = map (\col -> (row, col)) cols
            where 
                cols = [i | (i, c) <- zip [0..] line, c == '*']

allSymbols :: [String] -> Map Int [Int]
allSymbols inputs = fromList $ zip [0..] (map symbolIndices inputs)

adjacentToSymbol :: Part -> Map Int [Int] -> Bool
adjacentToSymbol part symbols = let
    (rowIndex, (start, end)) = (row part, indices part)
    validIndices = filter (`member` symbols) [(rowIndex-1)..(rowIndex+1)]
    possibleSymbols = map (symbols Map.!) validIndices
    in any (any (\i -> i >= start-1 && i <= end)) possibleSymbols

isAdjacentPart :: (Int, Int) -> Part -> Bool
isAdjacentPart (_, colIndex) part = let
    (start, end) = indices part
    in colIndex >= start - 1 && colIndex <= end

adjacentParts :: (Int, Int) -> Map Int [Part] -> [Part]
adjacentParts (rowIndex, colIndex) parts = let
    validIndices = filter (`member` parts) [(rowIndex-1)..(rowIndex+1)]
    possibleParts = concatMap (parts Map.!) validIndices
    in filter (isAdjacentPart (rowIndex, colIndex)) possibleParts

main :: IO ()
main = do
    input <- readFile "src/day3/input.txt"
    let allLines = lines input
    let allParts = parseInput allLines
    let symbols = allSymbols allLines
    print $ sum $ map number $ filter (`adjacentToSymbol` symbols) allParts

main2 :: IO ()
main2 = do
    input <- readFile "src/day3/input.txt"
    let allLines = lines input
    let allParts = parseInputToMap allLines
    let stars = allStars allLines
    let allAdjacentParts = map (`adjacentParts` allParts) stars
    let allGears = filter (\parts -> length parts == 2) allAdjacentParts
    print stars
    print allAdjacentParts
    print $ sum $ map (\[part1, part2] -> number part1 * number part2) allGears