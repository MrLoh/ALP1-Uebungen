-- 1
getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p []     = []
getUntil p (x:xs) | p x       = []
                  | otherwise = x:getUntil p xs


-- 2
freq :: (Eq a) => a -> [a] -> Int
freq k = foldl (\z x -> if x==k then 1+z else z) 0


-- 3a: Fibonacci
fibLR :: Integer -> Integer
fibLR 0 = 1
fibLR 1 = 1
fibLR n = fibLR (n-1) + fibLR (n-2)

fibER :: Integer -> Integer
fibER n = aux n (1,1)
          where
          aux n (a,b) | n<2       = a
                      | otherwise = aux (n-1) (a+b,a)

fibFL :: Integer -> Integer
fibFL n = fst (foldl (\(a,b) x -> (a+b,a)) (0,1) [0..n])

fibFR :: Integer -> Integer
fibFR n = fst (foldr (\x (a,b) -> (a+b,a)) (0,1) [0..n])


-- 3b: Fakultät
fakLR :: Integer -> Integer
fakLR 0 = 1
fakLR n = n*(fakLR (n-1))

fakER :: Integer -> Integer
fakER n = aux n 1
          where
          aux 0 akk = akk
          aux n akk = aux (n-1) (n*akk)

fakFL :: Integer -> Integer
fakFL n = foldl (\z x -> x*z) 1 [1..n]

fakFR :: Integer -> Integer
fakFR n = foldr (\x r -> x*r) 1 [1..n]


-- 3c: Summe von Quadraten
sumQuadLR :: [Integer] -> Integer
sumQuadLR []     = 0
sumQuadLR (x:xs) = x^2+(sumQuadLR xs)

sumQuadER :: [Integer] -> Integer
sumQuadER xs = aux xs 0
               where
               aux [] akk     = 0
               aux (x:xs) akk = aux xs (akk+x^2)

sumQuadFL :: [Integer] -> Integer
sumQuadFL = foldl (\z x -> z+x^2) 0

sumQuadFR :: [Integer] -> Integer
sumQuadFR = foldr (\x y -> x^2+y) 0


-- 4
filterLG :: (a -> Bool) -> [a] -> [a]
filterLG p xs = [ x | x <- xs, p x ]


-- 5
sumGerade :: [Integer] -> Integer
sumGerade = foldl (\z x -> if even x then z+x else z) 0

main = do
  print( freq 1 [1,1,0,2,4,1] )
  putStrLn ""
  print( getUntil (<1) [1,1,0,2,4,1] )
  putStrLn ""
  print( map fibLR [1..10] )
  print( map fibER [1..10] )
  print( map fibFL [1..10] )
  print( map fibFR [1..10] )
  putStrLn ""
  print( map fakLR [1..5] )
  print( map fakER [1..5] )
  print( map fakFL [1..5] )
  print( map fakFR [1..5] )
  putStrLn ""
  print( sumQuadLR [1..4] )
  print( sumQuadLR [1..4] )
  print( sumQuadFL [1..4] )
  print( sumQuadFR [1..4] )
  putStrLn ""
  print( filterLG (>1) [1,2,9,1,-1,4,0,1] )
  putStrLn ""
  print( sumGerade [1..10] )
