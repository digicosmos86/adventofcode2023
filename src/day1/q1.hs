import Data.Char (isDigit, ord)
import Data.List (find)
import Control.Monad (sequence)

findFirstDigit :: String -> Maybe Int
findFirstDigit str = case find isDigit str of
    Just c -> Just $ ord c - (ord '0')
    Nothing -> Nothing

findLastDigit :: String -> Maybe Int
findLastDigit = findFirstDigit . reverse

digitToNumber :: String -> Maybe Int
digitToNumber str = do
    a <- findFirstDigit str
    b <- findLastDigit str
    return $ a * 10 + b

main :: IO ()
main = do
    file <- readFile "src/day1/input.txt"
    print $ fmap sum $ sequence $ fmap digitToNumber $ lines file