-- * do block permits IO function to interact w/ the real world
--   It creates a clear frontier is created between pure and impure code.
-- * `<-` binds variable to an IO function.
-- * Binding is optional for example w/ `putStrLn`, we dont' need to bind on () (i.e. unit).
-- * You cannot bind on the last statement however.
-- * You can use a let with a do block to bind pure values

import Data.Char
main' = do
  putStrLn "Hello, what's your first name?"
  fn <- getLine
  putStrLn "What's your last name?"
  ln <- getLine
  let upFn = map toUpper fn
      upLn = map toUpper ln
  putStrLn $ "hey " ++ upFn ++ " " ++ upLn ++ ", how are you?"


-- I/O actions can only be performed within other I/O action:
-- 1. from the main function
-- 2. from a `do`

-- Use `return` as the last statement to return a value from main
-- return () if you don't want to return anything
-- `return` here does not "break" the execution flow
main = do
  line <- getLine
  if null line then
    return ()
  else do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- The "BOX" analogy is that
-- "<-" take a value from the I/O Box and binds it to a pure variable
-- "return" take a pure variable and puts it in a I/O Box
main2 = do
  a <- return "hell" -- Seems to be the bridge to returning I/O
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b
-- ^ doing the above is redundant in this specific example and a let clause would have sufficed
main3 = do
  let a = "hell"
      b = "yeah!"
  putStrLn $ a ++ " " ++ b

-- Conclusion
-- * We use return to return a value from an I/O Block.
-- * It must be the last statement executed from the I/O Block, otherwise it is ignored
-- * return "haha", would return the string
-- * return (), would return nothing since it's the unit value
