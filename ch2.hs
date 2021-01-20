-- To query type in haskell REPL:
--   :t <variable>


-- Type variable (generic type)
--   :t head
--   :: [a] -> a

-- Int is a 64bit integer
-- Integer is a large Int
factorial :: Integer -> Integer
factorial n = product [1..n]


-- Type Class
-- Sidenote: Function only with symbol use infix notation by default
--     :t (==)
--     (==) :: Eq a => a -> a -> Bool
--     Eq a means all types that satisfy Eq type class
-- Some example of type classes:
--     Eq  : a == b, a /= b
--     Ord : a < b, a >= b)
--     Show : a -> String
--     Read : String -> a
--     Enum: Bounded ordered list w/ succ and pred function. Can be used for range.
--     Bounded: Defines a min and max value
--     Num: Represent a numbers
--     Float: Represent floating-point numbers
--     Integral: Represent integers
-- . A type can be part of many type classes
-- . A type class may have many types
--
-- Type class dependencies: Sometimes type 'a' must be part of typeclass T before
--                          being part of typeclass S, we'll say T is prerequisite for S



-- Read needs to infer the return type from the context or be explicit specified
--  read "True" || False
--  read "True" :: Bool

