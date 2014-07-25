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
negative bs = (collapse bs) `addition` [0,0,0,0,0,0,0,1]

collapse :: Bits -> Bits
collapse []     = []
collapse (b:bs) | b == 0 = 1:(negative bs)
                | b == 1 = 0:(negative bs)

subtrakt :: Bits -> Bits -> Bits
subtrakt a b = addition a (negative b)

--produkt :: Bits -> Bits -> [Int]

main = do
 print( n )
 print( collapse n )
 print( negative n )
 print( bin2dec (negative (negative n)) )
 where n = decTo8bit 10


