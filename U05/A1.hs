qsort :: [[a]] -> [[a]]
qsort []     = []
qsort (l:ls) = qsort [ a | a <- ls, length a < length l ] ++ [l]
                     ++ qsort [ b | b <- ls, length b >= length l ]

main = do
 print( qsort [[7,2,3], [10], [1,2], []] )
