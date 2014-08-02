data Number = Bin [Int] | Hex [Int] | Okt [Int]
              deriving (Show, Eq)

okt2bin :: Number -> Number
okt2bin (Okt is) = Bin (o2bLi is)
                   where
                   b8s :: [[Int]]
                   b8s = [ [a,b,c] | a<-[0,1], b<-[0,1], c<-[0,1] ]
                   o2bLi :: [Int] -> [Int]
                   o2bLi [] = []
                   o2bLi (i:is)
                         | i>7       = error "not a regular okt"
                         | otherwise = (b8s!!i) ++ (o2bLi is)

bin2hex :: Number -> Number
bin2hex (Bin is) = Hex (reverse (b2hLi (reverse is) 0 0))
                   where
                   b2hLi :: [Int] -> Int -> Int -> [Int]
                   b2hLi [] h curr = [curr]
                   b2hLi (i:is) h curr
                         | i>1       = error "not a regular bin"
                         | h<4       = b2hLi is (h+1) (curr+(2^h)*i)
                         | otherwise = curr:(b2hLi (i:is) 0 0)

okt2hex :: Number -> Number
okt2hex = bin2hex.okt2bin

main = do
  print( okt2bin o1 )
  print( okt2bin o2 )
  print( bin2hex b1 )
  print( bin2hex b2 )
  print( bin2hex b3 )
  print( okt2hex o2 )
  where
  b1 = Bin [0,1,0,1]
  b2 = Bin [0,1,0,1 , 0,1,0,1]
  b3 = Bin [0,1 , 0,1,1,0 , 1,0,1,0 , 1,0,0,1]
  o1 = Okt [2,7,4]
  o2 = Okt [7,7,7]

