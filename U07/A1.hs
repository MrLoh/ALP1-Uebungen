split :: [a] -> [[a]] --O(n)
split []     = []
split [x]    = [[x]]
split (x:xs) = [x]:(split xs)

merge :: (Ord a) => [a] -> [a] -> [a] --O(n+m)
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) | x <= y    = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

mergeLists :: (Ord a) => [[a]] -> [[a]] --O(n)
mergeLists []       = []
mergeLists [x]      = [x]
mergeLists (x:y:xs) = (merge x y): mergeLists xs

mergeSort :: (Ord a) => [[a]] -> [[a]] --O(n*log(n))
mergeSort [x] = [x]
mergeSort xs  = mergeSort (mergeLists xs) --wird log(n) mal aufgerufen

startMergeSort :: (Ord a) => [a] -> [a] --O(n*log(n))
startMergeSort xs = sortedList
                    where
                    [sortedList] = mergeSort (split xs) --O(n*lo(n))
