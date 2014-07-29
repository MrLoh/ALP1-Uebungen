myMin :: (Ord a) => [a] -> a
myMin (x:xs) = foldr min x xs

main = do
 print( myMin [2,3,5,1] )
 print( myMin "abc0" )
