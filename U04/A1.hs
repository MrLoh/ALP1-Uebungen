--mymax :: (Double,Double,Double) -> Double
--mymax (a,b,c) | a >= b && a >= c = a
--              | b >= c           = b
--              | otherwise        = c

--maximum :: [Double] -> Double
--maximum [] = error "no maximum of empty list"
--maximum [x] = x
--maximum (x:y:xs) | x>y       = max (x:xs)
--                 | otherwise = max (y:xs)

rgb2cymk :: (Int,Int,Int) -> (Double,Double,Double,Double)
rgb2cymk (0,0,0) = (0.0,0.0,0.0,1.0)
rgb2cymk (r,g,b) = (c,m,y,k)
                   where
                   w = maximum [rd/255.0, gd/255.0, bd/255.0]
                   c = (w-(rd/255.0))/w
                   m = (w-(gd/255.0))/w
                   y = (w-(bd/255.0))/w
                   k = 1.0-w
                   rd = fromIntegral r
                   gd = fromIntegral g
                   bd = fromIntegral b

main = do
 print( rgb2cymk (255,255,255) )
 print( rgb2cymk (0,0,0) )
