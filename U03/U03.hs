import Data.List
--sort :: [Int] -> [Int]
--sort []     = []
--sort (x:xs) = (sort lesser) ++ [x] ++ (sort greater)
--              where
--              lesser  = filter (< x) xs
--              greater = filter (>= x) xs

--Aufgabe 1
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


--Aufgabe 2
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


--Aufgabe 3
groupEquals :: [Int] -> [[Int]]
groupEquals []     = [[]]
groupEquals (x:xs) = greq [] [x] xs
                     where
                     greq :: [[Int]] -> [Int] -> [Int] -> [[Int]]
                     greq out ys []     = out ++ [ys]
                     greq out ys (x:xs) | (head ys) == x  = greq out (x:ys) xs
                                        | otherwise       = greq (out ++ [ys]) [x] xs


--Aufgabe 4
myZip :: [Int] -> [Int] -> [(Int,Int)]
myZip []     []     = []
myZip (x:xs) []     = error "missmatched lists"
myZip []     (y:ys) = error "missmatched lists"
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)


--Aufgabe 5
type Bits = [Int]
--we will further assume, that length Bits == 8
decTo8bit :: Int -> Bits
decTo8bit n = fill (dec2bin n)
              where
              fill b | (length b) > 8 = drop ((length b)-8) b
                     | (length b) < 8 = fill (0:b)
                     | otherwise      = b

dec2bin :: Int -> Bits
dec2bin n | n < 2     = [n]
          | otherwise = dec2bin (n`div`2) ++ [n`mod`2]

bin2dec :: Bits -> Int
bin2dec b = b2d (reverse b)
            where
            b2d []     = 0
            b2d (x:xs) = 2*(b2d xs) + x

addition :: Bits -> Bits -> Bits
addition as bs = add (reverse as) (reverse bs) [] 0
                 where
                 add :: Bits -> Bits -> Bits -> Int -> Bits
                 add []     bs     out ut = out
                 add (a:as) (b:bs) out ut | (a+b+ut) == 3 = add as bs (1:out) 1
                                          | (a+b+ut) == 2 = add as bs (0:out) 1
                                          | otherwise     = add as bs ((a+b+ut):out) 0

negative :: Bits -> Bits
negative bs = (neg bs) `addition` [0,0,0,0,0,0,0,1]
              where
              neg [] = []
              neg (b:bs) | b == 0 = 1:(negative bs)
                         | b == 1 = 0:(negative bs)

subtrakt :: Bits -> Bits -> Bits
subtrakt a b = addition a (negative b)

--produkt :: Bits -> Bits -> [Int]



--Aufgabe 6
type Set = [Int]
--we will further assume, that Sets have no double elements and are sorted
arr2set :: [Int] -> [Int]
arr2set l = sort (mks l)
            where
            mks :: [Int] -> [Int]
            mks []     = []
            mks (x:xs) | (elem x xs) = mks xs
                       | otherwise   = x : (mks xs)

elementOf :: Int -> Set -> Bool
elementOf e []       = False
elementOf e (el:set) | e == el   = True
                     | otherwise = elementOf e set

gleich :: Set -> Set -> Bool
gleich [] []                 = True
gleich (el1:set1) (el2:set2) | el1 == el2 = gleich set1 set2
                             | otherwise  = False
gleich _ _                   = False

teilmenge :: Set -> Set -> Bool
teilmenge []        set2 = True
teilmenge (el:set1) set2 | el `elementOf` set2 = teilmenge set1 set2
                         | otherwise           = False

vereinigung :: Set -> Set -> Set
vereinigung set1 set2 = mks (set1 ++ set2)
                        where
                        mks :: Set -> Set
                        mks []     = []
                        mks (x:xs) | (elem x xs) = mks xs
                                   | otherwise   = x : (mks xs)

schnittmenge :: Set -> Set -> Set
schnittmenge []   set2      = []
schnittmenge set1 []        = []
schnittmenge (el:set1) set2 | el `elementOf` set2 = el:(schnittmenge set1 set2)
                            | otherwise           = schnittmenge set1 set2

mengendifferenz :: Set -> Set -> Set
mengendifferenz []   set2      = set2
mengendifferenz set1 []        = set1
mengendifferenz (el:set1) set2 | el `elementOf` set2 = schnittmenge set1 set2
                               | otherwise           = el:(schnittmenge set1 set2)






