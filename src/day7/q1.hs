import Data.List (group, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, (!))
import Text.Regex.Posix

order :: Map Char Int
order = fromList $ zip "23456789TJQKA" [1 ..]

newtype Card = Card Char deriving (Show, Eq)

instance Ord Card where
  compare (Card a) (Card b) = compare (order ! a) (order ! b)

data CamelHand
  = FiveOfAKind [Card]
  | FourOfAKind [Card]
  | FullHouse [Card]
  | ThreeOfAKind [Card]
  | TwoPairs [Card]
  | OnePair [Card]
  | HighCard [Card]
  deriving (Show, Eq)

compareCamel :: [Card] -> [Card] -> Ordering
compareCamel (x : xs) (y : ys) = case compare x y of
  EQ -> compareCamel xs ys
  x -> x
compareCamel [] _ = EQ
compareCamel _ [] = EQ

instance Ord CamelHand where
  compare hand1 hand2 = case (hand1, hand2) of
    (FiveOfAKind a, FiveOfAKind b) -> compareCamel a b
    (FiveOfAKind _, _) -> GT
    (_, FiveOfAKind _) -> LT
    (FourOfAKind a, FourOfAKind b) -> compareCamel a b
    (FourOfAKind _, _) -> GT
    (_, FourOfAKind _) -> LT
    (FullHouse a, FullHouse b) -> compareCamel a b
    (FullHouse _, _) -> GT
    (_, FullHouse _) -> LT
    (ThreeOfAKind a, ThreeOfAKind b) -> compareCamel a b
    (ThreeOfAKind _, _) -> GT
    (_, ThreeOfAKind _) -> LT
    (TwoPairs a, TwoPairs b) -> compareCamel a b
    (TwoPairs _, _) -> GT
    (_, TwoPairs _) -> LT
    (OnePair a, OnePair b) -> compareCamel a b
    (OnePair _, _) -> GT
    (_, OnePair _) -> LT
    (HighCard a, HighCard b) -> compareCamel a b

parseHand :: String -> CamelHand
parseHand s = case cardCounts of
  [(5, a)] -> FiveOfAKind cards
  [(4, a), (1, b)] -> FourOfAKind cards
  [(3, a), (2, b)] -> FullHouse cards
  [(3, a), (1, b), (1, c)] -> ThreeOfAKind cards
  [(2, a), (2, b), (1, c)] -> TwoPairs cards
  [(2, a), (1, b), (1, c), (1, d)] -> OnePair cards
  [(1, a), (1, b), (1, c), (1, d), (1, e)] -> HighCard cards
  _ -> error "Invalid hand"
  where
    cards = map Card (s =~ "[23456789TJQKA]+" :: String)
    sortedCards = sortBy (flip compare) $ cards
    cardCounts = sortBy (flip compare) $ map (\x -> (length x, head x)) $ group sortedCards

parseLine :: String -> (CamelHand, Int)
parseLine s = (parseHand first, read second :: Int)
  where
    (first : second : _) = splitOn " " s

main :: IO ()
main = do
  contents <- readFile "src/day7/input.txt"
  let hands = map parseLine $ lines contents
  let ranks = zip [1 ..] $ sortBy (\(x, _) (y, _) -> x `compare` y) hands
  let scores = map (\(rank, (_, bid)) -> (rank, bid)) ranks
  print $ sum $ map (uncurry (*)) scores
