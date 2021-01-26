-- Ch 6 -  Modules
-- import Data.List -- Imports all functions in Data.List
-- import Data.List (nub, sort) -- Import only nub, sort
-- import Data.List hiding (nub) -- Import all except nub
import qualified Data.List as L
import qualified Data.Char as C
import Data.Maybe
import qualified Data.Map as Map
import qualified Geometry.Cube as Cube

-- nub: Remove duplicates
numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

--- wordFreq
wordFreq :: String -> [(String, Int)]
wordFreq = map (\l -> (head l, length l)) . L.group . L.sort . L.words


-- contains
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' la = all (\(x, y) -> x == y) . zip la

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn sub = any (sub `L.isPrefixOf`) . L.tails


-- Caesar's cipher
caesarEncode :: Int -> String -> String
caesarEncode i =  map (C.chr . (+i) . C.ord)
caesarDecode :: Int -> String -> String
caesarDecode i = caesarEncode (negate i)


-- foldl vs foldl' (lazy vs strict)
--   foldl will not resolve an expression until it is fully necessary. So expansion will resolve
--   to a big stack of deferred computation equivalent to:
--     foldl (+) 0 [1,2,3] ===
--       foldl (+) (0 + 1) [2,3]
--       foldl (+) ((0 + 1) + 2) [3]
--       foldl (+) (((0 + 1) + 2) + 3) [] -- Now here expression is resolved
--       (((0 + 1) + 2) + 3)
--       ((1 + 2) + 3)
--       (3 + 3)
--       6
--
--   foldl' looks like a tail-recursive representation, probably because the z term being strict
--   gets evaluated at every application of f:
--     foldl' (+) 0 [1, 2, 3] ===
--     foldl' (+) 1 [2, 3]
--     foldl' (+) 3 [3]
--     foldl' (+) 6 []
--     6


digitSum :: Int -> Int
digitSum = sum . map C.digitToInt . show
fstNumSum40 = head [i | i <- [1..], let ds = digitSum i, ds == 40]
fstNumSum40' = L.find (\i -> digitSum i == 40) [1..]


-- ### Mapping keys to Values
-- Solution1: Association List: [(k, v)]
--   - Works well for small sets
--   - Not ordered, so O(n) for access and update
phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  , ("penny", "823-2322")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey k = fmap snd . L.find (\(k1, _) -> k1 == k)
findKey' k = foldr (\ (k1, v) acc -> if k1 == k then Just v else acc) Nothing
findKey'' k [] = Nothing
findKey'' k ((k1, v):xs)
  | k == k1 = Just k
  | otherwise = findKey'' k xs

-- Non-duplicate Map type
phoneBookMap :: Map.Map String String
phoneBookMap = Map.fromList phoneBook
-- Map.lookup to find an entry
-- Map.size for size
toDigits :: String -> [Int]
toDigits = map C.digitToInt. filter C.isDigit

-- Maps over values
phoneBookDigitsMap = Map.map toDigits phoneBookMap

-- Deal w/ duplication
phoneBookDupMap = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) phoneBook
-- Keep in mind that the concatenation function can do anything to merge the values
-- ex. max, sum, count, etc.


cubeArea = Cube.area 3.0
