-- Ch 6 -  Modules
-- import Data.List -- Imports all functions in Data.List
-- import Data.List (nub, sort) -- Import only nub, sort
-- import Data.List hiding (nub) -- Import all except nub
import qualified Data.List as L

-- nub: Remove duplicates
numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

--- wordFreq
wordFreq :: String -> [(String, Int)]
wordFreq = map (\l -> (head l, length l)) . L.group . L.sort . L.words
