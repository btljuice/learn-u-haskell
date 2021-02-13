import Control.Exception
import Data.Char
import Data.List
import System.IO
import System.Directory
import System.Environment
import System.Random
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS

-- getContents returns the whole stdin pipe
-- Once compiled, you can invoke this program the following way at the shell
-- > ./ch9 < haiku.txt

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

main1 = do
  contents <- getContents
  putStr $ (shortLinesOnly contents)

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- interact is the equivalent of main1 above
main2 = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = unlines . map (\s -> if isPalindrome s then "A Palindrome" else "not a Palindrome") . lines


-- Now it's time to read from a file. Simply echo the file at the output
main3 = do
  fileHandle <- openFile "input/girlfriend.txt" ReadMode
  contents <- hGetContents fileHandle
  putStr contents
  hClose fileHandle

-- Same as below, but handles the hClose automatically
main3' = do
  withFile "input/girlfriend.txt" ReadMode (\handle ->
    do
      contents <- hGetContents handle
      putStr contents)

-- Under the hood, it uses a the bracket function.
-- Bracket is used for typical scenario of acquire / release resource where
-- Some cleanup is needed.
-- Resource release is analog to a "ensure/finally" clause.
withFile' filename mode f = bracket (openFile filename mode) (hClose) f
main3'' = do
  withFile' "input/girlfriend.txt" ReadMode (\handle ->
    do
      contents <- hGetContents handle
      putStr contents)


-- hGetContents. Many I/O functions working on stdin/stdout have their h<prefixed> function counterparts
-- to work on file / I/O Handle


-- Because onetime read/write/append file exists, there's helper function for those as well
main4 = do
  contents <- readFile "input/girlfriend.txt"
  writeFile "output/girlfriendcaps.txt" (map toUpper contents)
  putStr contents

-- TODO List
addTodoMain = do
  todoItem <- getLine
  appendFile "output/todo.txt" (todoItem ++ "\n")

deleteTodoMain = do
  contents <- readFile "output/todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TODO items:"
  mapM_ putStrLn numberedTasks
  numberString <- getLine
  let number = read numberString :: Int
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt")

-- Reading arguments at the command line
main5 = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are: "
  mapM_ putStrLn args
  putStrLn "The program name is: "
  putStrLn progName


-- Randomness
-- System.random works like in scala functional programming book:
-- Each call is stateless, and returns both the new value and the new state as a tuple
someRandomNum :: (Int, StdGen)
someRandomNum = random (mkStdGen 100)

-- randoms is like an infinite random stream, you take as much as you need.
-- The drawback is that it does not provide the seed as a return value. The goal is probably
-- to initiate the stream and just pull from it whenever a random value is needed
threeCoins' :: [Bool]
threeCoins' = take 3 $ randoms (mkStdGen 1)

-- Here's our custom finiteRandom, that pick n random value, and returns the RandomGen value
finiteRandom :: (RandomGen g, Random a, Num n, Eq n, Ord n) => n -> g -> ([a], g)
finiteRandom n g
  | n == 0 = ([], g)
  | n < 0 = error "n must be >= 0"
  | otherwise = (h:t, gn)
  where (h, g1) = random g
        (t, gn) = finiteRandom (n-1) g1

threeCoins :: [Bool]
threeCoins = fst $ finiteRandom 3 (mkStdGen 1)

-- For a random value within a range, you can:
dieRoll :: (Int, StdGen)
dieRoll = randomR (1, 6) (mkStdGen 1)

-- Finally randomRs for a random stream of a certain range
randString = take 10 $ randomRs ('a', 'z') (mkStdGen 3)


-- getStdGen will return the same gen while newStdGen will return a new generator
main6 = do
  gen <- getStdGen -- Use for the global generator
  putStrLn $ take 20 (randomRs ('a', 'z') gen)


-- Converts lazy list to semi-lazy byte string
lazyPacking = LBS.pack [98..120]
lazyUnpacking = LBS.unpack lazyPacking
strictPacking = SBS.pack [98..108]
fromStrictToLazy = LBS.cons 85 $ LBS.fromChunks [strictPacking, strictPacking, strictPacking ]


-- Copy program w/ lazy operations
-- Only 2 differences is in the Stream operations
mainCopy = do
  (fileName1:fileName2:_) <- getArgs
  copy fileName1 fileName2

copy source dest = do
  contents <- LBS.readFile source -- 1st lazy operator
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      LBS.hPutStr tempHandle contents -- 2nd lazy operator
      hClose tempHandle
      renameFile tempName dest)

-- Key take here is that in most circumstances you can use strict VS lazy streams interchangeably
-- depending of your needs
