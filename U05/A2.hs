binSearch :: (Ord a) => a -> [a] -> Bool
binSearch b []  = False
binSearch b [x] = b==x
binSearch b xs  | b<mitte  = binSearch b links   --O(log n) aufrufe
                | b>mitte  = binSearch b rechts
                | b==mitte = True
                where
                half = (length xs) `div` 2  --O(n)
                links = take half xs        --O(n)
                rechts = drop half xs       --O(n)
                mitte = head rechts         --O(n)

--insgesamt also O(n*log n)

main = do
 print( binSearch 3 [1,2,3,4,5,6] )
 print( binSearch 10 [1,2,3,4,5,6] )
