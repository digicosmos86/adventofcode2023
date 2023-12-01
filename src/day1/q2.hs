import Data.List (findIndex, isPrefixOf, tails)
import Data.Char (isDigit, ord)
import Control.Applicative (liftA2)

numbers::[String] = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
reverseNumbers::[String] = map reverse numbers

findString :: String -> String -> Maybe Int
findString str substr = findIndex (isPrefixOf substr) (tails str)

reverseFindString :: String -> String -> Maybe Int
reverseFindString str substr = do
    pos <- findString (reverse str) substr
    return $ (length str) - pos - 1

firstStringText :: String -> [Maybe Int]
firstStringText str = map (findString str) numbers

lastStringText :: String -> [Maybe Int]
lastStringText str = map (reverseFindString str) reverseNumbers

findFirstDigit :: String -> Maybe Int
findFirstDigit = findIndex isDigit

findLastDigit :: String -> Maybe Int
findLastDigit str = do 
    pos <- findFirstDigit $ reverse str
    return $ (length str) - pos - 1

cmp :: (Int -> Int -> Bool) -> (Maybe Int, Char) -> (Maybe Int, Char) -> (Maybe Int, Char)
cmp comp (a, ai) (b, bi) = case (a, b) of
    (Nothing, Nothing) -> (Nothing, ai)
    (Nothing, Just _) -> (b, bi)
    (Just _, Nothing) -> (a, ai)
    (Just a', Just b') -> if comp a' b' then (a, ai) else (b, bi)

minIndex :: String -> (Maybe Int, Char)
minIndex str = foldr (cmp (<)) (Nothing, '0') $ zip (firstStringText str) ("0123456789"::[Char])

maxIndex :: String -> (Maybe Int, Char)
maxIndex str = foldr (cmp (>)) (Nothing, '0') $ zip (lastStringText str) ("0123456789" :: [Char])

firstDigit :: String -> Maybe Int
firstDigit str = do
    c <- case (a, b) of
        (Just a', Just b') -> if a' < b' then Just ai else Just (str !! b')
        (Just a', Nothing) -> Just ai
        (Nothing, Just b') -> Just (str !! b')
        (Nothing, Nothing) -> Nothing
    return $ (ord c) - (ord '0')
    where (a, ai) = minIndex str
          b = findFirstDigit str
        
lastDigit :: String -> Maybe Int
lastDigit str = do
  c <- case (a, b) of
        (Just a', Just b') -> if a' > b' then Just ai else Just (str !! b')
        (Just a', Nothing) -> Just ai
        (Nothing, Just b') -> Just (str !! b')
        (Nothing, Nothing) -> Nothing
  return $ (ord c) - (ord '0')
  where
    (a, ai) = maxIndex str
    b = findLastDigit str

digitToNumber :: String -> Maybe Int
digitToNumber str = do
  a <- firstDigit str
  b <- lastDigit str
  return $ a * 10 + b
  
main :: IO ()
main = do
    file <- readFile "src/day1/input.txt"
    print $ fmap sum $ sequence $ fmap digitToNumber $ lines file