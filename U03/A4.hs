myZip :: [Int] -> [Int] -> [(Int,Int)]
myZip []     ys     = []
myZip xs     []     = []
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

main = do
 print( myZip [1,2,3] [2,4,6] )
 print( myZip [1,2,3] [2,4,6,8,10] )
