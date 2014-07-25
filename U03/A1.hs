random :: Int -> Int
random seed =((25173*seed + 13849) `mod` 65536) `mod` 10000

randList :: Int -> [Int]
randList n = rali n [random n]
             where
             rali :: Int -> [Int] -> [Int]
             rali n xs | (length xs) == n = xs
                       | otherwise        = rali n (a:xs)
                                            where
                                            a = random (head xs)

randUntilRepeat :: Int -> [Int]
randUntilRepeat seed = rali [random seed]
                       where rali xs = let a = random (head xs) in if notElem a xs
                                                                   then rali (a:xs)
                                                                   else xs

main = do
 print( randUntilRepeat 100 )
 print( randUntilRepeat 1 )
