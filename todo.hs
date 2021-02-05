import System.Environment
addTodo :: String -> String -> IO()
addTodo fn todo = do
  putStrLn $ "addTodo " ++ fn ++ " " ++ todo

viewTodo :: String -> IO()
viewTodo fn =
  putStrLn $ "viewTodo " ++ fn

removeTodo :: String -> Int -> IO()
removeTodo fn num = do
  putStrLn $ "removeTodo " ++ fn ++ " " ++ show num


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
