-- Input is l_a1, l_b1, l_c1, l_a2, l_b2, l_c2, ...
-- l_a1 : Length of segment on road A, connecting a0 - a1
-- l_b1 : Length of segment on road B, connecting b0 - b1
-- l_c1 : Length of crossing segment, connecting a1 - b1

-- Algorithm
-- path_a = 0 // smallest length to reach a(i-1)
-- path_b = 0 // smallest length to reach b(i-1)
-- For each segment (i)
--    path_a = min( path_a + a(i) , path_b + b(i) + c(i) )
--    path_b = min( path_b + b(i) , path_a + a(i) + c(i) )

shortest :: [(Int, Int, Int)] -> (Int, Int)
shortest = foldl calc (0, 0)
  where calc (pa, pb) (a, b, c) = (min (pa + a) (pb + b + c), min (pb + b) (pa + a + c))
