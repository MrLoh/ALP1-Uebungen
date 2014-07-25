paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)


square :: (Int, Int, Int) -> Char
square (x,y,s) = if y>x
                    then if x>(-y)+s
                         then 'B'
                         else '.'
                    else if (y<n || y>2*n && y<m) && (x>m) || (x>m+2*n || x>m && x<m+n ) && y<m
                         then '*'
                         else ' '
                         where
                         m = s`div`2
                         n = s`div`6

chessboard :: (Int, Int, Int) -> Char
chessboard (x,y,s) = if (y-1)`mod`8<4 && (x-1)`mod`8<4 || (y-1)`mod`8>=4 && (x-1)`mod`8>=4
                     then '█'
                     else ' '



easteregg :: (Int, Int, Int) -> Char
easteregg (x,y,s) = if 1.0 > ( ((xd-r)/r)^2 + ((yd-r)/r)^2 )
                    then if x<m
                         then if y`mod`6==0 && x`mod`3==0
                              then '@'
                              else '_'
                         else if y`mod`8<4
                              then '█'
                              else '='
                    else if x>m+m`div`2
                         then '|'
                         else ' '
                    where
                    m = s`div`2
                    r = (fromIntegral s)/2.0
                    xd = fromIntegral x
                    yd = fromIntegral y

main = do
 putStrLn (paintPicture square 30)
 putStrLn (paintPicture chessboard 40)
 putStrLn (paintPicture easteregg 50)

