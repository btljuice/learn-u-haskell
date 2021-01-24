-- Higher order function
-- All Haskell function are curried by default

mult3 :: (Num a) => a -> a -> a -> a
mult3 x y z = x*y*z

-- compare 10 gives a Int -> Ordering function because of currying
compareTo10 = compare 10
-- ex. compareTo10 9

-- Sections
-- By putting a symbol function in parenthesis, you get the curried function
-- Exception to this rule is the unary operation - (ex. -4).
-- Use the substract function instead
divideBy10 = (/10)
hundredDividedBy = (100/)

-- section works  for infix notation also
isAlphaChar :: Char -> Bool
isAlphaChar = (`elem` ['a'..'z'])


-- N.B. Be careful about the following saying:
-- - Partial "Application" of a function: (when a function was supplied not all of its parameter)
--   ex. mult3 2 3
-- - Partial Function: Function that only map partially the domain of its input
--   ex. plus3 x =
--           | x > 6 = x +3

-- Take function as parameter
twice :: (a -> a) -> a -> a
twice f a = f (f a)
-- twice (++ "FOO") "BAR"
-- twice ("FOO" ++) "BAR"


-- Higher order function as input
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = g where g y x = f x y

myFlipSimpler :: (a -> b -> c) -> b -> a -> c
myFlipSimpler f y x = f x y

-- Functional programmer's toolbox
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f(x) = x : filter' f xs
  | otherwise = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' lt ++ [x] ++ quicksort' gt
  where lt = filter (<x) xs
        gt = filter (>= x) xs


-- Nice example of lazy evaluation.
-- Q find the largest number divisible by 3829 under 100 000
-- Nice trick here is head, so the filtered infinite list only gets evaluated until first
-- element multiple of 3829 is found.
answer = head (filter p [100000, 99999..])
  where p i = i `mod` 3829 == 0
-- Remember that list comprehension are just map + filter under the hood
answer' = head [i | i <- [100000, 99999..], i `mod` 3829 == 0]

firstWord = takeWhile (/= ' ') "elephants know how to party"

-- Find sum of odd squares < 10 000
-- takeWhile will stop evaluating expression when the retrieved number >= 10000
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum' = sum (takeWhile (<10000) [i*i | i <- [1..], odd (i*i)])

-- Collatz Chain
-- Start with a natural number
-- if n == 1 stop
-- if n is even => n /2
-- if n is odd => 3*n + 1
-- repeat until stop.
-- Math theory tells it should finish always at 1
-- Question: for all chain generated w/ [1..100] as a starting point, which have length > 15 ?
-- Memoization could be used here for previous generated chains, but let's implement this the
-- naive way
collatz :: Int -> [Int]
collatz n
      | n <= 0 = error "Only on strict positive Natural Number"
      | n == 1 = [1]
      | even n = n : collatz (n `div` 2)
      | odd n  = n : collatz (3*n + 1)

collatzAnswer = length [1 | i <- [1..100], length (collatz i) > 15]
collatzAnswer' = length (filter (> 15) (map (length . collatz) [1..100]))

-- Mapping functions w/ multiple parameterj
-- map (*) [0..1] will return a list of Int -> Int, waiting to be evaluated

-- Lambdas
-- '\' is used as it reminds the \ in the lambda symbol
collatzAnswer'' = length (filter (\ xs -> length xs > 15) (map collatz [1..100]))
-- Can take as many parameter as you need
--    zipWith (\a b -> (a*30 +3)/b) listA listB
-- Can use pattern matching
--   map (\(a,b) -> a + b) [(0,1), (1,2), (2, 3), (3, 5)]

-- If lambda is not surrounded by parentheses, all right expression belongs to it.
gimmickyAdd3 :: Int -> Int -> Int -> Int
gimmickyAdd3 = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- Foldleft
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

reverse' :: [a] -> [a]
reverse' = foldl (flip' (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\ x acc -> if f x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- Fold is a successive application of function:
--       foldl f === (f ... (f (f (f z a0) a1) a2) ... an)
-- OR if we flipped the parameters of f
--       foldl f === (f an ...(f (f a2 (f a1 (f a0 z))))
-- Look how similar it is to foldr
--       foldr f === (f a0 ...(f (f an-2 (f an-1 (f an z))))

-- - The key advantage of foldr combined w/ lazyness (e.g. scala Stream)
--   is that if the second parameter of f is not evaluated, then we can
--   early out of the successive applicatons.
-- - The key advantage of foldl in classical implementations is that it's tail-recursive
--   But in haskell the key concept is guarded recursion (yet to know what it means)

-- Right fold on infinite list
and' :: [Bool] -> Bool
and' = foldr1 (&&) -- === (a0 && (a1 && (a2 && ...)))
-- and' ((replicate 100 True) ++ (repeat False)) -- This returns False
