import Data.List.Split (splitOn)
import Data.List (sortBy)
import Text.Regex.Posix

type Seed = Int
type Source = Int
type Target = Int
type Range = Int

type MapItem = (Target, Source, Range)
type Map = Seed -> [MapItem] -> Int

mapSeed :: Map
mapSeed seed [] = seed
mapSeed seed ((target, source, range):xs) =
    if seed >= source && seed < source + range
    then target + (seed - source)
    else mapSeed seed xs

mapSeeds :: Seed -> [[MapItem]] -> Int
mapSeeds = foldl mapSeed

scanSeeds :: Seed -> [[MapItem]] -> [Int]
scanSeeds = scanl mapSeed

parseNumber x = read x :: Int

parseMapItem :: String -> MapItem
parseMapItem str = case map parseNumber $ splitOn " " str of
    a:b:c:rest -> (a,b,c)

parseMap :: String -> [MapItem]
parseMap str = map parseMapItem $ drop 1 $ lines str

parseFile :: String -> ([Seed], [[MapItem]])
parseFile file = let
    seedsText:rest = splitOn "\n\n" file
    seeds = map parseNumber (getAllTextMatches (seedsText =~ "[0-9]+") :: [String])
    maps = map parseMap rest in
    (seeds, maps)

seedRange :: [Int] -> [(Int, Int)]
seedRange (x:y:xs) = (x, y) : seedRange xs
seedRange [] = []

inRange :: Int -> [(Int, Int)] -> Bool
inRange seed ((min, range):xs) = (seed >= min && seed < min + range) || inRange seed xs
inRange seed [] = False

mapSeedReverse :: Map
mapSeedReverse seed [] = seed
mapSeedReverse seed ((source, target, range) : xs) =
    if seed >= source && seed < source + range
      then target + (seed - source)
      else mapSeedReverse seed xs

mapSeedsReverse = foldl mapSeedReverse

scanSeedsReverse = scanl mapSeedReverse

findLocationInRange :: [Int] -> [[MapItem]] -> [(Int, Int)] -> Maybe Int
findLocationInRange [] mapItems range = Nothing
findLocationInRange (x:xs) mapItems range = let seedNumber = mapSeedsReverse x mapItems in
    if inRange seedNumber range 
    then Just x
    else findLocationInRange xs mapItems range

findLocationInRanges :: [(Int, Int, Int)] -> [[MapItem]] -> [(Int, Int)] -> Maybe Int
findLocationInRanges [] mapItems range = Nothing
findLocationInRanges ((source, _, range1):xs) mapItems range = 
    case findLocationInRange [source..(source+range1)] mapItems range of
        Nothing -> findLocationInRanges xs mapItems range
        Just seedNumber ->  Just seedNumber

main::IO()
main = do
    contents <- readFile "src/day5/input.txt"
    let (seeds, maps) = parseFile contents
    let locations = map (`mapSeeds` maps) seeds
    print $ minimum locations

main2::IO()
main2 = do
  contents <- readFile "src/day5/input.txt"
  let (seeds, maps) = parseFile contents
  let range = seedRange seeds
  let reverseMaps = reverse maps
  let locations = sortBy (\(x, _, _) (y, _, _) -> x `compare` y) $ head reverseMaps
--   print $ reverseMaps
--   print $ scanSeedsReverse 82 reverseMaps
--   print $ scanSeedsReverse 43 reverseMaps
--   print $ scanSeedsReverse 86 reverseMaps
--   print $ scanSeedsReverse 35 reverseMaps
  print $ findLocationInRange [0..] reverseMaps range
