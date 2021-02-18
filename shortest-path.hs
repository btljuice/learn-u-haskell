-- Input is l_a1, l_b1, l_c1, l_a2, l_b2, l_c2, ...
-- l_a1 : Length of segment on road A, connecting a0 - a1
-- l_b1 : Length of segment on road B, connecting b0 - b1
-- l_c1 : Length of crossing segment, connecting a1 - b1

-- Algorithm
-- path_a = 0 // smallest length to reach a(i-1)
-- path_b = 0 // smallest length to reach b(i-1)
-- For each segment (i)
--    path_a = min( path_a + a(i) , path_b + b(i) + c(i) )
--    path_b = min( path_b + b(i) , path_a + a(i) + c(i) )

import Data.List

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section  5 90 20
                   , Section 40  2 25
                   , Section 10  8  0
                   ]

roadStep :: ((Path, Int), (Path, Int)) -> Section -> ((Path, Int), (Path, Int))
roadStep ((pa, la), (pb, lb)) (Section a b c) =
  let new_pa1 = la + a
      new_pa2 = lb + b + c
      new_pb1 = lb + b
      new_pb2 = la + a + c
      sol_a = if new_pa1 <= new_pa2 then ((A, a):pa, new_pa1) else ((C, c):(B, b):pb, new_pa2)
      sol_b = if new_pb1 <= new_pb2 then ((B, b):pb, new_pb1) else ((C, c):(A, a):pa, new_pb2)
  in (sol_a, sol_b)

optimalPath :: RoadSystem -> (Path, Int)
optimalPath rs =
  let ((pa, la), (pb, lb)) = foldl roadStep (([], 0), ([], 0)) rs
  in if la <= lb then (reverse pa, la) else (reverse pb, lb)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      (path, pathTime) = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime
