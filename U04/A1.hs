type RGB = (Int, Int, Int)
type CYMK = (Float, Float, Float, Float)

--maximum :: [Double] -> Double
--maximum [] = error "no maximum of empty list"
--maximum [x] = x
--maximum (x:y:xs) | x>y       = maximum (x:xs)
--                   | otherwise = maximum (y:xs)

rgb2cymk :: RGB -> CYMK
rgb2cymk (0,0,0) = (0.0,0.0,0.0,1.0)
rgb2cymk (r,g,b) = (c,m,y,k)
                   where
                   c = getCol r
                   m = getCol g
                   y = getCol b
                   k = 1.0-w
                   w :: Float
                   w = maximum (map div255 [r,g,b])
                   div255 :: Int -> Float
                   div255 a = ((fromIntegral a)/255)::Float
                   getCol :: Int -> Float
                   getCol n = (w-div255 n)/w


main = do
 print( rgb2cymk (255,255,255) )
 print( rgb2cymk (0,0,0) )
