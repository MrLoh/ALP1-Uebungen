querSum :: Int -> Int
querSum n | n < 0     = - querSum (-n)
          | n < 10    = n
          | otherwise = querSum (n`div`10) + (n`mod`10)

prime :: Int -> Bool
prime n = pr n 2
          where
          pr :: Int -> Int -> Bool
          pr n k | n == k       = True
                 | n`mod`k == 0 = False
                 | otherwise    = pr n (k+1)

querSumPrimes :: Int -> [Int]
querSumPrimes n | n<2                              = []
                | (prime n) && (prime (querSum n)) = (querSumPrimes (n-1)) ++ [n]
                | otherwise                        = querSumPrimes (n-1)

querSumPrimes2 :: Int -> [Int]
querSumPrimes2 n = [ x | x <- primes, prime (querSum x)  ]
                   where
                   primes = [ a | a <- [2..n], prime a ]


main = do
 print( querSumPrimes 100 )
 print( querSumPrimes2 100 )
