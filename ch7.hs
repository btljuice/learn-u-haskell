module Shapes
( Point(..) -- (..) here exports all values within Point
, Shape(..)
, area
, nudge
, baseCircle
, baseRect )
where
-- ^ What's not exported above is not exposed (e.g. private)

-- Point, Shapes example
data Point = Point Float Float deriving (Show)
data Shape =
  Circle Point Float |
  Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x0 y0) (Point x1 y1)) = (abs $ x1 - x0) * (abs $ y1 - y0)

-- ANSME: How to override the `+` operator on Points? (Probably a Num type class...)
nudge :: Shape -> Point -> Shape
nudge (Circle (Point x y) r) (Point a b) = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point a b) = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- ANSME: Why it's not possible to return the Circle type?
--        baseCircle :: Float -> Circle .
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)


-- Declare Person the boring way
data Person' = Person' String String Int Float String String deriving (Show)
firstName' :: Person' -> String
firstName' (Person' f _ _ _ _ _) = f
lastName' :: Person' -> String
lastName' (Person' _ l _ _ _ _) = l
-- ... etc

-- Declare Person the cool way
data Person = Person {
    firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
} deriving (Eq, Show, Read)

data Car = Car {
    company :: String
  , model :: String
  , year :: Int
} deriving (Eq, Show, Read)

-- ANSME: If I replace year w/ age, there will be a conflict for the age function. How to resolve such conflict?


-- a here is type parameter as in the generic functions
-- Maybe is called a type constructor
data Maybe' a = Nothing' | Just' a
-- Interesting, Haskell will type infer a declaration as permissive as possible
-- > Nothing'
--      Nothing' :: Maybe' a
-- > Just' 1
--      Just' 1 :: Num a => Maybe' a
-- Nothing' above is call a "polymorphic type", as it blends to all Maybe' a types
-- [] above is a "polymorphic type" as well. It can interact with all list types


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector a b c) (Vector x y z) = Vector  (a+x) (b+y) (c+z)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector a b c) (Vector x y z) = a*x + b*y + c*z

vmult :: (Num a) => a -> Vector a -> Vector a
a `vmult` (Vector x y z) = Vector (a*x) (a*y) (a*z)
 

-- deriving keyword above works just as the scala Deriving[] macro
-- (e.g. applies the operation to each element of the structure, to adhere to the type class)

-- Data types
data MyBool = False' | True' deriving (Ord) -- This enum will be ordered. False' < True'
