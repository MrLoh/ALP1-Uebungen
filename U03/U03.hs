import Data.List

--AUFGABE 1
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


--AUFGABE 2
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


--AUFGABE 3
groupEquals :: [Int] -> [[Int]]
groupEquals []     = [[]]
groupEquals (x:xs) = greq [] [x] xs
                     where
                     greq :: [[Int]] -> [Int] -> [Int] -> [[Int]]
                     greq out ys []     = out ++ [ys]
                     greq out ys (x:xs) | (head ys) == x  = greq out (x:ys) xs
                                        | otherwise       = greq (out ++ [ys]) [x] xs


--AUFGABE 4
myZip :: [Int] -> [Int] -> [(Int,Int)]
myZip []     []     = []
myZip (x:xs) []     = error "missmatched lists"
myZip []     (y:ys) = error "missmatched lists"
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)





--AUFGABE 7




main = putStrLn ""
