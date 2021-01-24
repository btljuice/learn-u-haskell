-- Ch.4 Recursion
-- Side note: Tail recursion is a bit complicated, because all operations are lazy
-- In Haskell, you are more concern about "guarded recursion" (Concept to explore)
-- Tail recursion can be achieve by forcing strictness w/ `$!` operator
-- See https://stackoverflow.com/questions/13042353/does-haskell-have-tail-recursive-optimization
-- For the moment I'll focus on simple recursion w/o tail-recursion

myMax :: (Ord a) => [a] -> a
myMax [] = error "Max needs at least one argument"
myMax [i] = i
myMax [i,j] = if i > j then i else j
myMax (i:j:s) = myMax (myMax [i,j] : s)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = a : myReplicate (n-1) a

-- Tail recursion example w/ where
myWhereReplicate :: Int -> a -> [a]
myWhereReplicate n a = impl n []
  where impl i acc
          | i <= 0 = acc
          | otherwise = impl (i-1) $! (a:acc)

-- Tail recursion example w/ let
myLetReplicate :: (Num i, Ord i) => i -> a -> [a]
myLetReplicate n a =
  let impl i acc
        | i <= 0 = acc
        | otherwise = impl (i-1) $! (a:acc)
  in impl n []

myTake :: (Num i, Ord i) => i -> [a] -> [a]
myTake n _ | n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myRepeat :: a -> [a]
myRepeat a = a:myRepeat a

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (a:as) (b:bs) = (a,b) : myZip as bs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myQuicksort :: (Ord a) => [a] -> [a]
myQuicksort [] = []
myQuicksort (h:t) = myQuicksort lt ++ [h] ++ myQuicksort ge
  where lt = [x | x <- t, x < h]
        ge = [x | x <- t, x >= h]
