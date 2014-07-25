groupEquals :: [Int] -> [[Int]]
groupEquals []     = [[]]
groupEquals (x:xs) = greq [] [x] xs
                     where
                     greq :: [[Int]] -> [Int] -> [Int] -> [[Int]]
                     greq out ys []     = out ++ [ys]
                     greq out ys (x:xs) | (head ys) == x  = greq out (x:ys) xs
                                        | otherwise       = greq (out ++ [ys]) [x] xs

main = do
 print( groupEquals [1,1,2,1,2,2,1,1,1] )
