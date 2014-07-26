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
addition as bs | lnas >= lnbs = add (reverse as) (reverse (bs`padleft`(lnas-lnbs))) [] 0
               | otherwise    = add (reverse (as`padleft`(lnbs-lnas))) (reverse bs) [] 0
                 where
                 add :: Bits -> Bits -> Bits -> Int -> Bits
                 add []     []     out ut = out
                 add (a:as) (b:bs) out ut | (a+b+ut) == 3 = add as bs (1:out) 1
                                          | (a+b+ut) == 2 = add as bs (0:out) 1
                                          | otherwise     = add as bs ((a+b+ut):out) 0
                 lnas = length as
                 lnbs = length bs

negative :: Bits -> Bits
negative bs = (collapse bs) `addition` [0,0,0,0,0,0,0,1]

collapse :: Bits -> Bits
collapse []     = []
collapse (b:bs) | (b == 0) = 1:(collapse bs)
                | (b == 1) = 0:(collapse bs)

subtrakt :: Bits -> Bits -> Bits
subtrakt a b = addition a (negative b)

shiftleft :: Bits -> Int -> Bits
shiftleft bs 0 = bs
shiftleft bs n = shiftleft (tail(bs++[0])) (n-1)

padleft :: Bits -> Int -> Bits
padleft bs 0 = bs
padleft bs n = padleft (0:bs) (n-1)

produkt :: Bits -> Bits -> [Int]
produkt as bs = prd (as`padleft`8) (reverse (bs`padleft`8)) 0
                where
                prd as [] n    = []
                prd as (b:bs) n | (b == 1) = (as`shiftleft`n)`addition`(prd as bs (n+1))
                                | (b == 0) = prd as bs (n+1)

main = do
 print( n )
 print( m )
 putStrLn "  +/-"
 print( collapse n )
 print( negative n )
 print( bin2dec (negative (negative n)) )
 print( bin2dec (n`addition`m) )
 print( bin2dec (n`subtrakt`m) )
 putStrLn "  <</*"
 print( n )
 print( n`shiftleft`2 )
 print( n`produkt`m )
 print( bin2dec (n`produkt`m) )

 putStrLn ""
 where
  n = decTo8bit 10
  m = decTo8bit 4


