mergesortStart :: (Ord a) => [a] -> [a]
mergesortStart [ ] = mergesort 0 [ ]
mergesortStart xs  = mergesort (length xs) xs

merge :: (Ord a) => [a] -> [a] -> [a] --O(n+m)
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) | x <= y    = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

mergesort :: Integer -> [a]
mergesort _ []    = []
mergesort _ [x]   = [x]
mergesort _ [x,y] = if x <= y then [x,y] else [y,x]
mergesort len xs  = merge (mergesort h leftList) (mergesort (len-h) rightList)
                    where
                    h = len `div` 2
                    leftList = take h xs
                    rightList = drop h xs
