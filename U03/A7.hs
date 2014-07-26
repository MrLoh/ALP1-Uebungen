import Data.List

settify :: [Int] -> [Int]
settify = delDupes.sort

delDupes :: [Int] -> [Int]
delDupes []       = []
delDupes [x]      = [x]
delDupes (x:y:xs) | x==y      = delDupes (y:xs)
                  | otherwise = x:(delDupes (y:xs))

isprime :: Int -> Bool
isprime n | n < 2     = error "not defined for input < 2"
          | otherwise = pr n 2
                        where
                        pr :: Int -> Int -> Bool
                        pr n k | n == k       = True
                               | n`mod`k == 0 = False
                               | otherwise    = pr n (k+1)

listOfSums :: [Int] -> [Int]
listOfSums xs = [ a+b | a <- xs, b <- xs ]

primes :: Int -> [Int]
primes n = [ a | a <- [2..n], isprime a ]

primesums :: Int -> [Int]
primesums n = settify [ a | a <- listOfSums (primes n) ]

testGoldbachHypothesis :: Int -> Bool
testGoldbachHypothesis n | n<2        = error "defined for input < 2"
                         | n==2       = True
                         | n`mod`2==0 = (n `elem` primesums n) && (testGoldbachHypothesis (n-2))
                         | otherwise  = True && (testGoldbachHypothesis (n-1))
--this is not the brightest idea, it's very ineffective

goldbachPairs :: Int -> [(Int,Int)]
goldbachPairs x | x`mod`2 == 0 = [ (a,b) | a <- primes x, b <- primes x, a <= b && a+b == x ]
                | otherwise    = error "not an even number"

main = do
 print( listOfSums [1,2,0] )
 print( primes 100 )
 print( goldbachPairs 32 )
 print( testGoldbachHypothesis 100 )
