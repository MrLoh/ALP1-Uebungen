sumSquares :: Int -> Int
sumSquares n = foldr (+) 0 (map (^2) [1..n])

main = do
 print( sumSquares 4 )
 print( sumSquares 10 )
