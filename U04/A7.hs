paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)

diag (x,y,size) = if (x==y) then 'a' else ' '

quad (x, y, size) = if x>s && x<3*s && y>s && y<3*s
                    then ' '
                    else '+'
                    where
                          s = div size 4

gitter (x,y,size) = if k || p  then '0' else ' '
                    where
                         k = (mod x space)==0
                         p = (mod y space)==0
                         space = div size 5

flag (x,y,size) = if (x < (div size 3)) then 'M' else if (y < (div size 3)) then '8' else '.'



main = do
 putStrLn (paintPicture quad 20)
 putStrLn (paintPicture gitter 20)
 putStrLn (paintPicture flag 20)
 putStrLn (paintPicture diag 20)
