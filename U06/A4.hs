bin2dec :: [Int] -> Int
bin2dec (b:bs) = foldl addTimes2 0 bs
                 where
                 addTimes2 :: Int -> Int -> Int
                 addTimes2 z b = 2*z+b

main = do
 print( bin2dec [0,0,1,0] )
 print( bin2dec [0,1,0,0] )
 print( bin2dec [0,1,1,1] )
 print( bin2dec [0,0,0,0] )
