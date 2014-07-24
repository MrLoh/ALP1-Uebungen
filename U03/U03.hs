random :: Int -> Int
random seed =((25173*seed + 13849) `mod` 65536) `mod` 10000

randList :: Int -> [Int]
randList n = rali n [random n]
             where
              rali n xs | (length xs) == n = xs
                        | otherwise        = rali n (a:xs)
                                             where
                                               a = random (head xs)
