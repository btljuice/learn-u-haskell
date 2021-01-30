module Shapes
( Point(..) -- (..) here exports all values within Point
, Shape(..)
, area
, nudge
, baseCircle
, baseRect )
where
-- ^ What's not exported above is not exposed (e.g. private)

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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
data MyBool = False' | True' deriving (Eq, Ord) -- This enum will be ordered. False' < True'


-- Take note for example that read needs a type specification:
carRed :: Car
carRed = read "Car { company=\"Tesla\", model=\"T3\", year=4 }"


-- Day of the week
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Eq: Sunday == Monday => False
-- Ord: Sunday < Monday => True
-- Show: show Sunday => "Sunday"
-- Read: read "Sunday" :: Day => Sunday
-- Bounded: minBound :: Day => Sunday
--          maxBound :: Day => Saturday
-- Enum : [Thursday .. Saturday] => [Thursday, Friday, Saturday]
--        succ Thursday => Friday
--        pred Thursday => Wednesday

-- type alias
type MyString = [Char]
-- parametrized type synonyms
type AssocList k v = [(k, v)]

-- Either
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)
-- Again here data my have a polymorphic type:
-- :t Right 1 :: Num b => Either a b
-- ^ Here the minimal requirement:
--   b could fit any num type
--   a could fit any type


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either' String Code
lockerLookup id lockers  = toEither $ Map.lookup id lockers
    where toEither Nothing = Left' "Locker not found"
          toEither (Just (Taken, _)) = Left' "Locker taken"
          toEither (Just (Free, code)) = Right' code

lockerLookup' :: Int -> LockerMap -> Either' String Code
lockerLookup' id lockers  = case Map.lookup id lockers of Nothing -> Left' "Locker not found"
                                                          (Just (Taken, _)) -> Left' "Locker taken"
                                                          (Just (Free, code)) -> Right' code
lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken, "ZD39I"))
  ,(101,(Free, "JAH3I"))
  ,(103,(Free, "IQSA9"))
  ,(105,(Free, "QOTSA"))
  ,(109,(Taken, "893JJ"))
  ,(110,(Taken, "99292"))
  ]

-- Recursive data structure (List)
data List' a = Nil' | Cons' { listHead :: a, listTail :: (List' a) } deriving (Eq, Ord, Read, Show)
aList' = Cons' 1 . Cons' 2 $ Cons' 3 Nil'

infixr 5 :-:
data List'' a = Nil'' | a :-: (List'' a) deriving (Eq, Ord, Read, Show)
aList'' = 1 :-: 2 :-: 3 :-: Nil''

infixr 5 ^++
(^++) :: List'' a -> List'' a -> List'' a
Nil'' ^++ r = r
(a:-:as) ^++ r = a :-: (as ^++ r)


-- Recursive data structure (Tree)
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a EmptyTree = singleton a
treeInsert a n@(Node b l r)
  | a == b = n
  | a <= b = Node b (treeInsert a l) r
  | a > b  = Node b l (treeInsert b r)

treeElem :: (Ord a) => a -> Tree a -> Maybe (Tree a)
treeElem _ EmptyTree = Nothing
treeElem a n@(Node b l r)
  | a == b = Just n
  | otherwise = treeElem a n

treeContains :: (Ord a) => a -> Tree a -> Bool
treeContains x = Maybe.isJust . treeElem x


treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

-- Type classes
class Eq' a where
  (==!) :: a -> a -> Bool
  (/=!) :: a -> a -> Bool
  x ==! y = not (x /=! y) -- 2 values are equal IF they are not different
  x /=! y = not (x ==! y) -- 2 values are different IF they are not equal

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- Subclassing type classes (type class requirement/constraint)
-- class (Eq a) => Num a where
--   ...
-- Means that:  'a' type must be an instance of Eq to be eligible to be an instance of Num as well.


-- Parametrized Types as Instances of Type Classes
-- (ex. 'Maybe a' is a Parametrized Type / Type Constructor)
-- (ex. 'TrafficLight' is a concrete Type)
-- (ex. 'Maybe Char' is a concrete type)

-- You can't have a function like 'a -> Maybe', because the return type is not concrete.
-- That's why we can't do something like
--    instance Eq Maybe where
-- As `Maybe` is not a concrete type.
-- The solution
instance (Eq a) => Eq (Maybe' a) where
  Just' x == Just' y = x == y
  Nothing' == Nothing' = True
  _ == _ = False
-- Here only instances where a meets (Eq a) will adhere to the above type class
-- deriving creates such type of instance

instance (Ord a) => Ord (Maybe' a) where
  Nothing' <= Nothing' = True
  Nothing' <= Just' _ = True
  Just' x <= Just' y = x <= y
  _ <= _ = False


-- !!!! Most of the time !!!!
-- 1. Class constraints in class declarations
--       ex. class (Eq a) => Num a where
--           ...
--    Is to define that "Num is a subclass of Eq" (if a is Num, then a is Eq)
-- 2. Class constraints in instance declarations
--       ex. instance (Eq a) => Eq (Maybe' a) where
--           ..
--    Is to express requirements on type 'a'


-- Javascript YesNo
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

-- id stands for identify function
instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing  = False
  yesno (Just _) = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno Yellow = False
  yesno Green = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf p ifexp elexp = if yesno p then ifexp else elexp


-- Functor type class
-- NOTE: Notice that here, f is a type constructor / parametrized type.
--       the signature is the same than for a concrete type.
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- Here because f is a type constructor, we don't put an 'a' next to the type
instance Functor' Maybe' where
  fmap' _ Nothing' = Nothing'
  fmap' f (Just' x) = Just' (f x)

instance Functor' [] where
  fmap' = map


-- Usually types that act like a "box"/"container" can be a Functor, because one simply needs
-- to change all the containers values through f.
-- One thing we have to validate any invariants that are in the contract of type f

-- WARNING: Be careful though! If you use the Tree a type to represent a binary search tree,
-- there is no guarantee that it will remain a binary search tree after mapping a function over it.
instance Functor' Tree where
  fmap' _ EmptyTree = EmptyTree
  fmap' f (Node x l r) = Node (f x) (fmap' f l) (fmap' f r)

-- Interesting case. Because Either is a constructor w/ 2 types, only one can be bound, and the
-- other needs to be left "free".
instance Functor' (Either a) where
  fmap' _ (Left x) = Left x
  fmap' f (Right x) = Right (f x)

instance Functor' (Map.Map k) where
  fmap' = Map.map

-- * values are data computed at runtime
-- * values are annotated of a type, to know their possible representation, and how to manipulate
--   them
-- * types are annotated by some label called "kinds". We could say it's the "type of a type"
--   Look at :k command:
--   > :k Int
--     Int :: *
--     ^ Concrete Type
--   > :k Maybe
--     Maybe :: * -> *
--     ^ Type constructor w/ 1 parameter
--   > :k Maybe Int
--     Maybe Int :: *
--   > :k Either
--     Either :: * -> * -> *
--     ^ Type constructor w/ 2 parameters

-- This is analog to:
-- * > :t isUpper
--     Char -> Bool
--   > t: isUpper 'A'
--     Bool


-- Type constructors are "curried" just like functions
