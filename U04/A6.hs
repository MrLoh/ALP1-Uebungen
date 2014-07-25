nextCollatz :: Int -> Int
nextCollatz n | n`mod`2 == 0 = n`div`2
              | otherwise    = n*3+1

collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq n = n:collatzSeq (nextCollatz n)

collatzSeqs :: Int -> [[Int]]
collatzSeqs n = [ collatzSeq a | a <- [1..n] ]

main = do
 print( nextCollatz 5 )
 print( collatzSeq 10 )
 print( collatzSeqs 5 )
