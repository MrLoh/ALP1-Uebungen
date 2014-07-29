binSearch :: (Ord a) => a -> [a] -> Bool
binSearch b []  = False
binSearch b [x] = b==x
binSearch b xs  | b<mitte  = binSearch b links   --log(n) aufrufe
                | b>mitte  = binSearch b rechts
                | b==mitte = True
                where
                half = (length xs) `div` 2  --O(n)
                links = take half xs        --O(n)
                rechts = drop half xs       --O(n)
                mitte = head rechts         --O(n)

{-
n = length xs
In jedem Schritt O(n)
worst case: immer b<mitte
dann Aufrufe mit Listen der Länge n + n/2 + n/4 + n/8 + n/16 + ...
also Sum_i=1^(log(n)) n/2^i = (2-2^-log(n))*n
also O(n)
-}

main = do
 print( binSearch 3 [1,2,3,4,5,6] )
 print( binSearch 10 [1,2,3,4,5,6] )
