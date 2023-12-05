import Data.List.Split (splitOn)
import Text.Regex.Posix
import Text.Printf (printf)

data Bag = Bag {
    blue :: Int,
    red :: Int,
    green :: Int
} deriving (Show)

data Game = Game {
    gameID :: Int,
    bags :: [Bag]
} deriving (Show)

parseColor :: String -> String -> Int
parseColor color str =
    let pattern = (printf "([0-9]+) %s" color) :: String in
    case (str =~ pattern :: [[String]]) of
    [[_, num]] -> read num :: Int
    _ -> 0

parseBlue = parseColor "blue"
parseRed = parseColor "red"
parseGreen = parseColor "green"

parseBag :: String -> Bag
parseBag str = Bag {
    blue = parseBlue str,
    red = parseRed str,
    green = parseGreen str
}

parseBags :: String -> [Bag]
parseBags str = map parseBag (splitOn "; " str)
    
parseGame :: String -> Game
parseGame str = case splitOn ": " str of
    a:b:_ -> Game {
        gameID = parseId a,
        bags = parseBags b
    }
    where parseId str = case (str =~ "Game ([0-9]+)" :: [[String]]) of
            [[_, num]] -> read num :: Int
            _ -> 0

ref :: Bag
ref = Bag {
    blue = 14,
    red = 12,
    green = 13
}

isBagPossible :: Bag -> Bool
isBagPossible bag = (blue bag) <= (blue ref) 
    && (red bag) <= (red ref)
    && (green bag) <= (green ref)

isGamePossible :: Game -> Bool
isGamePossible game = all isBagPossible (bags game)

maxBag :: Bag -> Bag -> Bag
maxBag bag1 bag2 = Bag {
    blue = max (blue bag1) (blue bag2),
    red = max (red bag1) (red bag2),
    green = max (green bag1) (green bag2)
}

maxBags :: [Bag] -> Bag
maxBags bags = foldr1 maxBag bags

powerOfGame :: Game -> Int
powerOfGame game = maxBlue * maxRed * maxGreen
    where 
        bag = maxBags (bags game)
        maxBlue = max 1 (blue bag)
        maxRed = max 1 (red bag)
        maxGreen = max 1 (green bag)

main :: IO ()
main = do
    file <- readFile "src/day2/input.txt"
    let games = map parseGame (lines file)
    let possibleGames = filter isGamePossible games
    let possibleGameIds = map gameID possibleGames
    print $ sum possibleGameIds

main2 :: IO ()
main2 = do
    file <- readFile "src/day2/input.txt"
    let games = map parseGame (lines file)
    let powers = map powerOfGame games
    print $ sum powers