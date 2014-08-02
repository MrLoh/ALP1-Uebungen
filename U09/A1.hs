(\\) :: (Eq a) => [a] -> [a] -> [a]
xs \\ []     = xs
[] \\ ys     = []
(x:xs) \\ ys | x`inList`ys = (xs\\ys)
             | otherwise   = x:(xs\\ys)
             where
             inList :: (Eq a) => a -> [a] -> Bool
             x`inList`[]     = False
             x`inList`(y:ys) | x==y      = True
                             | otherwise = x`inList`ys

(\\\) :: (Eq a) => [a] -> [a] -> [a]
xs \\\ ys = [ x | x <- xs, ([] == [ y | y <- ys, x==y ]) ]

smallestNatNotIn :: [Int] -> Int
smallestNatNotIn ns = head ([ n | n <- [1..] ]\\ns)

{-
inList hat O(m), da im schlimmsten Fall jedes Element in ys einmal mit x verglichen wird. (m = length ys)

(\\) hat O(n^2), da im schlimmsten Fall für jedes Element in xs einmal inList aufgerufen wird. Dabei ist n das maximum aus length xs, length ys.

Damit hat smallestNatNotIn Komplexität O(n^2), dabei ist n das maximum aus length ns und dem Rückgabewert.
-}

main = do
  print( [2, 6, 1, 7, 0, 9] \\ [2, 0, 9] )
  print( [2, 6, 1, 7, 0, 9] \\\ [2, 0, 9] )
  print( smallestNatNotIn [3, 5, 2, 7, 6, 10, 0, 1, 4, 12] )
