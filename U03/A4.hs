myZip :: [Int] -> [Int] -> [(Int,Int)]
myZip []     []     = []
myZip (x:xs) []     = error "missmatched lists"
myZip []     (y:ys) = error "missmatched lists"
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

main = do
 print( myZip [1,2,3] [2,4,6] )
