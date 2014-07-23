import Data.Char

--Aufgabe 1
gZsum :: Integer -> Integer
gZsum 0 = 0
gZsum n | (n `mod` 2 == 0) = n + gZsum (n-1)
        | otherwise        = gZsum (n-1)

gZsumGauss :: Integer -> Integer
gZsumGauss n = let m = n`div`2 in m*(m+1)

--Aufgabe 2
arrow :: Int -> Int -> Int
arrow k n = aow k n k
            where aow ks 0 k = ks
                  aow ks n k = aow (ks^k) (n-1) k

--Aufgabe 3
dec2bin :: Int -> [Int]
dec2bin n | n<2       = [n]
          | otherwise = dec2bin (n`div`2) ++ [n`mod`2]

summ :: [Int] -> Int
summ []     = 0
summ (x:xs) = x + sum xs

binsumm :: Int -> Int
binsumm n = summ (dec2bin n)

--Aufgabe 4
int :: Char -> Int
int c = fromEnum c - 48

char :: Int -> Char
char i = toEnum (i+48)

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
oct2hex s = dec2hex (oct2dec s)

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
balanced:: [Char] -> Bool
balanced text = bal [] text
                where
                    bal:: [Char] -> [Char] -> Bool
                    bal [] []      = True
                    bal sta (c:cs) | elem c "({[<" = bal (c:sta) cs
                                   | elem c ")]}>" = if (mathParenthesis (head sta) c)
                                                     then bal (tail sta) cs
                                                     else False
                                   | otherwise     = bal sta cs
                    bal _ _        = False

mathParenthesis :: Char -> Char -> Bool
mathParenthesis '(' ')' = True
mathParenthesis '{' '}' = True
mathParenthesis '[' ']' = True
mathParenthesis '<' '>' = True
mathParenthesis  _   _  = False

--Aufgabe 8
--not quite finished
flatten :: [[Int]] -> [Int]
flatten []       = []
flatten (xs:xss) | xs == []  = []
                 | otherwise = let (y:ys) = xs in y : (flatten (ys:xss))





