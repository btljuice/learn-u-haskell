import Data.List.Split

calc :: [Double] -> String -> [Double]
calc (a:b:t) "+" = (b+a):t
calc (a:b:t) "-" = (b-a):t
calc (a:b:t) "*" = (b*a):t
calc (a:b:t) "/" = (b/a):t
calc t n = (read n):t

solveRPN :: [String] -> Double
solveRPN ss = impl [] ss
  where impl acc [] = head acc
        impl acc (s:t) = impl (calc acc s) t


test1 = solveRPN [ "10", "4", "3", "+", "2", "*", "-"]
