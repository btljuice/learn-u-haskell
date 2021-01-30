-- Neat REPL commands:
-- :h help
-- :t <expression> <= type
-- :doc <expression>
-- :info <expression>
-- :k <expression< <= kinds
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNum x = if x > 100 then x else 2*x

list = [1,2,3,4,5] -- Equivalent of 1:2:3:4:5:[]
listConcatenation = list ++ [6, 7, 8, 9, 10]
listCons = 0 : list
thirdElement = list !! 2 -- Indexed 0
listCompare = listCons < list  -- Compares in lexicographical order

-- List operation
-- head : [ ... tail ... ]
-- [ ... init ... ] : last : []
listHead = head list
listTail = tail list
listLast = last list
listInit = init list
listLength = length list
isNull = null list  -- returns True if list is empty
listReverse = reverse list
listTake2 = take 2 list
listDrop2 = drop 2 list
is4in = 4 `elem` list

-- Ranges
range20 = [1..20]  -- by default step is size 1
alphabet = ['a'..'z']
rangeWithSteps = [2,4..10] -- [2, 4, 6, 8, 10]
reverseRange = [5,4..1] -- [5, 4, 3, 2, 1]

-- Infinite lists
infiniteList = [1,2..]
cyclingList = cycle [1,2..5]
repeatList = repeat 6

-- Like a fill function
replicateList = replicate 3 10 -- [10, 10, 10]


-- List Comprehension
-- Akin to the set comprehension definition { 2*x | x E N, x <= 10 }
-- [ map | x <- list1, y <- list2, filter1, filter2, ...]
-- Multiple filter is possible
listComp = [ 2*x | x <- [1..10], 2*x >= 12, x /= 9] -- Scala: 1 to 10 map { 2*_ } filter { _ >= 12 }
-- Multiple list is possible
checker1D = [ even (x + y) | x <- [1..3], y <- [1..3]] -- [ 0,1, .. ]
-- You can put list comprehension within list comprehension
checker2D = [ [even (x + y) | x <- [1..3] ] | y <- [1..3] ]

-- Tuple
umbrellaLabels = [ "Number " ++ show n | n <- [1..7]]
umbrellaNames = ["Luther", "Diego", "Allison", "Klaus", "The Boy", "Ben", "Vanya"]
umbrellaTeam = zip3 [1..7] umbrellaLabels umbrellaNames
aTuple = (1, "Number One", "Luther", 3.5)

-- Side are integer within 1..10
-- Perimeter == 24
-- Right angle triangle
rightTriangle = [(x, y, z) | x <- [1..10], y <- [x..10], z <- [y..10], i + y + z == 24, x^2 + y^2 == z^2 ]
