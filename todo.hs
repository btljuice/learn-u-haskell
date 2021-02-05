import Control.Exception
import Data.List
import System.IO
import System.Environment
import System.Directory

addTodo :: String -> String -> IO()
addTodo fn todo = do
  appendFile fn (todo ++ "\n")

viewTodo :: String -> IO()
viewTodo fn = do
  contents <- readFile fn
  putStr contents

removeTodo :: String -> Int -> IO()
removeTodo fn num = do
  contents <- readFile fn
  let todoTasks = lines contents
      newTodoItems = unlines $ delete (todoTasks !! num) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fn
        renameFile tempName fn)

main = do
  args <- getArgs
  let action = args !! 0
      filename = args !! 1
  -- putStrLn $ "Action = " ++ show action
  -- putStrLn $ "Filename = " ++ filename
  case action of
    "add" -> addTodo filename (args !! 2)
    "view" -> viewTodo filename
    "remove" -> removeTodo filename (read $ args !! 2)
