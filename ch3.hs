-- Pattern matching
draw2Dice :: Int -> String
draw2Dice 7 = "Win"
draw2Dice _ = "Lose" -- variables that start w/ a lower case act as a "catchall"

-- If you don't have a catchall, non-exhaustive pattern match will throw an exception
diceNumber :: Int -> String
diceNumber 1 = "One"
diceNumber 2 = "Two"
diceNumber 3 = "Three"
diceNumber 4 = "Four"
diceNumber 5 = "Five"
diceNumber 6 = "Six"

-- Define factorial recursively
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)

-- Tuple pattern match
addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (i0, j0) (i1, j1) = (i0 + i1, j0 + j1)

-- Wildcard for unused parameter
third :: (a, b, c) -> c
third (_, _, c) = c

-- Pattern match in list comprehension
xs = [(1,3), (4, 3), (2,4), (5,3), (5,6), (3,1)]
sumXs = [a+b | (a,b) <- xs]

-- Pattern match on list
myHead :: [a] -> a
myHead (a:_) = a
myHead [] = error "No head on empty list"

-- Pattern match on list 2
tell :: (Show a) => [a] -> String
tell [] = "This is empty list"
tell [a] = "List of one element: " ++ show a
tell [a, b] = "List of 2 elements: " ++ show a ++ ", " ++ show b
tell (a:b:c:_) = "List of >2 elements: " ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ "..."

-- @ pattern : Same as in scala
firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter s@(c:_) = "First letter: of string " ++ s ++ "is " ++ show c

-- Use the guard and where clause
bmiTell :: Double -> Double -> String
bmiTell w h
  | bmi <= skinny = "Underweight"
  | bmi < normal = "Normal"
  | bmi == normal = "Perfect"
  | bmi <= overweight = "Overweight"
  | otherwise = "Obese"
  where bmi = w/h^2
        [skinny, normal, overweight] = [18.5, 25.0, 30.0] -- Pattern matching here as well

-- Pattern match in where clause; where clause is in the function scope
initials :: String -> String -> [Char]
initials firstName lastName = [f, l]
  where (f:_) = firstName
        (l:_) = lastName
-- Where clause acts as local variables

-- You can define a local function in the where clause
calcBmis :: [(Double, Double)] -> [Double]
calcBmis hws = [bmi w h | (w, h) <- hws]
  where bmi w h = w/h^2

-- Let vs Where: There almost the same, but differ in style
-- Let => Expression
--   As it is an expression, it can be inserted locally pretty much everywhere
-- Where => Declaration
--   It's usually at the function level, as shown in the examples above
-- See https://wiki.haskell.org/Let_vs._Where for more differences
initialsLet :: String -> String -> String
initialsLet firstName lastName =
  let (f:_) = firstName
      (l:_) = lastName
  in [f, l]

calcBmisLet :: [(Double, Double)] -> [Double]
calcBmisLet hws =
  let bmi w h = w/h^2
  in [bmi w h | (w, h) <- hws]

-- Let is an expression so it can be inserted everywhere there's an expression
letComputation = 4 * (let a = 9 in a/(a+1)) + 2
-- Let can use the pattern matching
letList = [let (a, b, c) = (1, 2, 3) in a+b+c]
-- Let in list comprehension
---  Take note that the `in` keyword is not used here
letListComprehension = [bmi | (w, h) <- [(85, 1.9), (100,1.65)], let bmi = w/h^2, bmi > 1]


-- case expressions.
-- Akin to the pattern matching done at the function signature level, but can be used like an expression
describeList :: [a] -> String
describeList l = "The list is " ++ case l of [] -> "empty."
                                             [_] -> "a singleton list."
                                             _ -> "a longer list."

describeList2 :: [a] -> String
describeList2 l = "This list is " ++ what l
  where what [] = "empty."
        what [_] = "a singleton list."
        what _ = "a longer list."
