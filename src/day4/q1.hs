import Data.List.Split (splitOn)
import Data.Bits (toIntegralSized)
import Text.Regex.Posix
import GHC.Stats (RTSStats(copied_bytes))

data Card = Card {
    card_id :: Int,
    winning :: [Int],
    numbers :: [Int]
} deriving (Show)

score :: Card -> Int
score Card{ winning=winning, numbers=numbers } = 
    score' winning numbers 0
    where
        score' winning numbers total = case numbers of
            (x:xs) -> let winning' = filter (/=x) winning in
                if length winning' == length winning
                    then score' winning' xs total
                    else score' winning' xs (if total == 0 then 1 else total * 2)
            [] -> total

scoreWithCopy :: Card -> (Int, Int)
scoreWithCopy Card{ winning=winning, numbers=numbers } = 
    score' winning numbers 0 0
    where
        score' winning numbers total copies = case numbers of
            (x:xs) -> let winning' = filter (/=x) winning in
                if length winning' == length winning
                    then score' winning' xs total copies
                    else score' winning' xs (if total == 0 then 1 else total * 2) (copies + 1)
            [] -> (total, copies)

scoresWithCopies :: [(Int, Int)] -> Int
scoresWithCopies (x:rest) = case x of
    (card_score, copies) -> card_score + sum (map fst (take copies rest)) + scoresWithCopies rest
scoresWithCopies [] = 0

cardInstances :: [Int] -> [Int] -> [Int]
cardInstances (x:xs) (y:ys) = case splitAt x ys of
    (before, after) -> y:cardInstances xs (map (+y) before ++ after)
cardInstances [] _ = []

parseCard :: String -> Card
parseCard str = case splitOn ": " str of
    [a,b] -> case splitOn " | " b of
        [c,d] -> Card {
            card_id = parseId a,
            winning = parseNumbers c,
            numbers = parseNumbers d
        }
    where 
        parseId str = read (str =~ "[0-9]+" :: String) :: Int
        parseNumbers str = map (\num -> read num :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String])

main :: IO ()
main = do
    input <- readFile "src/day4/input.txt"
    let cards = map parseCard $ lines input
    let scores = map score cards
    print $ sum scores

main2 :: IO ()
main2 = do
    input <- readFile "src/day4/input.txt"
    let cards = map parseCard $ lines input
    let (_, copies) = unzip $ map scoreWithCopy cards
    let n_cards = cardInstances copies (repeat 1)
    print $ sum n_cards