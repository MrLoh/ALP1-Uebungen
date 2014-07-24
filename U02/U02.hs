import Data.Char

int :: Char -> Int
int c = fromEnum c - 48

char :: Int -> Char
char i = toEnum (i+48)


--Aufgabe 1
gZsum :: Integer -> Integer
gZsum 0 = 0
gZsum n | n < 0            = error "not defined for negative Integers"
        | (n `mod` 2 == 0) = n + gZsum (n-1)
        | otherwise        = gZsum (n-1)

gZsumGauss :: Integer -> Integer
gZsumGauss n = let m = n`div`2 in m*(m+1)


--Aufgabe 2
arrow :: Integer -> Integer -> Integer
arrow k 1 = k
arrow k n = k^(arrow k (n-1))


--Aufgabe 3
dec2bin :: Int -> [Int]
dec2bin n | n<2       = [n]
          | otherwise = dec2bin (n`div`2) ++ [n`mod`2]

summ :: [Int] -> Int
summ []     = 0
summ (x:xs) = x + sum xs

binsumm :: Int -> Int
binsumm n = summ (dec2bin n)

-- alternativ direct queerSum funktion, für beliebige Basis b
querSum :: Int -> Int -> Int
querSum n b | n < 0 = - querSum (-n) b
            | n < b = n
            | otherwise = (querSum (n`div`b) b) + (n`mod`b)


--Aufgabe 4
oct2dec :: String -> Int
oct2dec ""     = 0
oct2dec (o:os) = int o * 8^(length os) + oct2dec os

dec2hex :: Int -> String
dec2hex n | n<16      = [hexChar n]
          | otherwise = dec2hex (n`div`16) ++ [hexChar (n`mod`16)]

hexChar :: Int -> Char
hexChar n | n<10    = char n
          | n == 10 = 'A'
          | n == 11 = 'B'
          | n == 12 = 'C'
          | n == 13 = 'D'
          | n == 14 = 'E'
          | n == 15 = 'F'

oct2hex :: String -> String
oct2hex = dec2hex.oct2dec

-- alternativ über Binärzahlen
oct2bin :: String -> String
oct2bin ""     = ""
oct2bin (x:xs) = [ [a,b,c] | a <- bits, b <- bits, c <- bits ]!!(int x) ++ (oct2bin xs)
                 where bits = "01"
                 -- generiert Liste ["000","001","010","011","100","101","110","111"]

bin2hex :: String -> String
bin2hex ""               = ""
bin2hex (b3:b2:b1:b0:xs) = ["0123456789ABCDEF"!!i] ++ bin2hex xs
                           where i = int b3 * 2^3 + int b2 * 2^2 + int b1 * 2 + int b0

padding :: String -> String
padding xs = [ '0' | x <- [1..( (length xs)`mod`4)] ]

oct2hex2 :: String -> String
oct2hex2 = bin2hex.padding.oct2bin


--Aufgabe 5
sumDigits :: Int -> Int
sumDigits n | (length (dec2arr n) == 1) = n
            | otherwise               = sumDigits (sumArr (dec2arr n))

sumArr :: [Int] -> Int
sumArr []     = 0
sumArr (x:xs) = x + sumArr xs

dec2arr :: Int -> [Int]
dec2arr n | n<10      = [n]
          | otherwise = dec2arr (n`div`10) ++ [n`mod`10]


--Aufgabe 6
multLists :: [Int] -> [Int] -> [Int]
multLists [] []         = []
multLists [] (y:ys)     = error "non matching list lengths"
multLists (x:xs) []     = error "non matching list lengths"
multLists (x:xs) (y:ys) = (x*y) : (multLists xs ys)


--Aufgabe 7
--not quite finished
balanced :: [Char] -> Bool
balanced text = bal [] text
                where
                  bal:: [Char] -> [Char] -> Bool
                  bal []   []        = True
                  bal sta ('(':cs)   = bal (')':sta) cs
                  bal sta ('[':cs)   = bal (']':sta) cs
                  bal sta ('{':cs)   = bal ('}':sta) cs
                  bal (k:sta) (c:cs) | notElem c ")}]" = bal (k:sta) cs
                                     | k == c       = bal sta cs
                                     | otherwise    = False
                  bal sta (c:cs)     | elem c ")}]" = False
                                     | otherwise    = bal sta cs
                  bal (k:sta) ""     = False


--Aufgabe 8
flatten :: [[Int]] -> [Int]
flatten []       = []
flatten (xs:xss) = xs ++ (flatten xss)


--Aufgabe 9
makeSet :: [Int] -> [Int]
makeSet []     = []
makeSet (x:xs) | (elem x xs) = makeSet xs
               | otherwise   = x : (makeSet xs)


