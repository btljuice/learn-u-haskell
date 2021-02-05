import Control.Exception
import Data.Char
import Data.List
import System.IO
import System.Directory
import System.Environment

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
