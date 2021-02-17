solveRPN :: String -> Double
solveRPN = head . foldl calc [] . words
  where calc :: [Double] -> String -> [Double]
        calc (a:b:t) "+" = (b+a):t
        calc (a:b:t) "-" = (b-a):t
        calc (a:b:t) "*" = (b*a):t
        calc (a:b:t) "/" = (b/a):t
        calc (a:b:t) "^" = (b ** a):t
        calc (a:t) "ln" = log a : t
        calc t "sum" = [sum t]
        calc t n = (read n):t

test1 = solveRPN "10 4 3 + 2 * -"
test2 = solveRPN "90 34 12 33 55 66 + * - +"
