maximum :: [Int] -> Int
maximum [] = error "no maximum of empty list"
maximum [x] = x
maximum (x:y:xs) | x>y       = max (x:xs)
                 | otherwise = max (y:xs)

mymax :: (Double,Double,Double) -> Double
mymax (a,b,c) | a >= b && a >= c = a
              | b >= c           = b
              | otherwise        = c

rgb2cymk :: (Int,Int,Int) -> (Double,Double,Double,Double)
rgb2cymk (0,0,0) = (0.0,0.0,0.0,1.0)
rgb2cymk (r,g,b) = (c,m,y,k)
                   where
                   w = mymax ((fromIntegral r)/255.0,(fromIntegral g)/255.0,(fromIntegral b)/255.0)
                   c = (w-((fromIntegral r)/255.0))/w
                   m = (w-((fromIntegral g)/255.0))/w
                   y = (w-((fromIntegral b)/255.0))/w
                   k = 1.0-w

main = do
 print( rgb2cymk (255,255,255) )
 print( rgb2cymk (0,0,0) )
