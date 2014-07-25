echtTeiler :: Int -> [Int]
echtTeiler n = [ a | a <- [1..n-1], n`mod`a==0 ]

main = do
 print( echtTeiler 250 )
