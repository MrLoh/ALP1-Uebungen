{-
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f ( f x )

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x):(map f xs)

int2bin :: Int -> [Int]
int2bin n | n == 0     = []
          | otherwise = int2bin (n`div`2) ++ [n`mod`2]
-}

unfold p f g x | p x       = []
               | otherwise = (f x):(unfold p f g (g x))


iterat f = unfold (\x -> False) (\x -> x) f

mapp f = unfold (\xs -> xs==[]) (\(x:xs) -> f x) (\(x:xs) -> xs)

int2bin = revers.unfold (\n -> n==0) (\n -> mod n 2) (\n -> div n 2)
          where
          revers = foldl (\xs z -> z:xs) []

main = do
  print( take 7 (iterat (^2) 2) )
  print( mapp (+1) [1..10] )
  print( int2bin 10 )
