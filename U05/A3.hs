positions1 :: (Eq a) => a -> [a] -> [Int]
positions1 k xs = pos k xs 0 []
                  where
                  pos :: (Eq a) => a -> [a] -> Int -> [Int] -> [Int]
                  pos k []     n out = out
                  pos k (x:xs) n out | k==x      = pos k xs (n+1) (out++[n])
                                     | otherwise = pos k xs (n+1) out

positions2 :: (Eq a) => a -> [a] -> [Int]
positions2 k xs = [ n | n <- [0..((length xs)-1)], (xs!!n)==k ]

positions3 :: (Eq a) => a -> [a] -> [Int]
positions3 k xs = filter isel [ n | n <- [0..((length xs)-1)] ]
                 where
                 isel :: Int -> Bool
                 isel n = xs!!n==k

main = do
 print( positions1 'a' "Maria Antonieta" )
 print( positions1 'a' "Blub" )
 print( positions2 'a' "Maria Antonieta" )
 print( positions2 'a' "Blub" )
 print( positions3 'a' "Maria Antonieta" )
 print( positions3 'a' "Blub" )
