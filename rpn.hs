solveRPN :: String -> Double
solveRPN expression = head $ foldl calc [] (words expression)
  where calc :: [Double] -> String -> [Double]
        calc (a:b:t) "+" = (b+a):t
        calc (a:b:t) "-" = (b-a):t
        calc (a:b:t) "*" = (b*a):t
        calc (a:b:t) "/" = (b/a):t
        calc t n = (read n):t

test1 = solveRPN "10 4 3 + 2 * -"
