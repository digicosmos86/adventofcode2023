import Text.Regex.Posix
import Data.Char (isDigit)

options :: Int -> [(Int, Int)]
options time = [(x, x * (time - x)) | x <- [1..(time-1)]]

options1 :: Int -> [(Int, Int)]
options1 time = [(x, x * (time - x)) | x <- [14 .. (time - 14)]]

parseNumbers :: String -> [Int]
parseNumbers s = map read (getAllTextMatches (s =~ "[0-9]+") :: [String])

validGames :: (Int, Int) -> [(Int, Int)]
validGames (time, distance) = filter (\(_, d) -> d >= distance) $ options time

validGames1 :: (Int, Int) -> [(Int, Int)]
validGames1 (time, distance) = filter (\(_, d) -> d >= distance) $ options1 time


main :: IO ()
main = do
    contents <- readFile "src/day6/input.txt"
    let (firstLine:secondLine:_) = lines contents
    let times = parseNumbers firstLine
    let distances = parseNumbers secondLine
    let allValidGames = zipWith (curry validGames) times distances
    print $ product $ map length allValidGames

main1 :: IO ()
main1 = do
    contents <- readFile "src/day6/input.txt"
    let (firstLine:secondLine:_) = lines contents
    let time = read (filter isDigit firstLine) :: Int
    let distance = read (filter isDigit secondLine) :: Int
    print $ length $ validGames1 (time, distance)