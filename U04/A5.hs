echtTeiler :: Int -> [Int]
echtTeiler n = [ a | a <- [1..n-1], n`mod`a==0 ]

mysum :: [Int] -> Int
mysum []     = 0
mysum (x:xs) = x+(mysum xs)

amicable :: Int -> Int -> Bool
amicable n m | n == m    = False
             | otherwise = n == mysum (echtTeiler m) && m == mysum (echtTeiler n)

main = do
 print( amicable 220 284 )
