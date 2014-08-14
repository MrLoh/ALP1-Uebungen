{-#  LANGUAGE NPlusKPatterns  #-}
import Data.Function

{-
zero = \s z -> z
one = \s z -> s z
suc = \n s z -> s (n s z )
true = \t f -> t
false = \t f -> f
nott = \b -> b false true
iszero = \n -> n false (nott false)
-}

add :: Integer -> Integer -> Integer
add = fix (\r n m -> if n==0 then m else r (n-1) (m+1))

mult :: Integer -> Integer -> Integer
mult = fix (\r n m -> if n==0 then 0 else add m (r (n-1) m) )

pot2 :: Integer -> Integer
pot2 = fix (\r n -> if n==0 then 1 else mult 2 (r (n-1)))

coc :: (b -> c) -> (a -> b) -> (a -> c)
coc = \f g x -> f(g x)

reversR :: [a] -> [a]
reversR = fix (\r xs -> if (length xs)==0 then [] else (r (tail xs))++[head xs] )

reversL :: [a] -> [a]
reversL = foldl (\zs x -> x:zs) []

f :: Integer -> Integer
f = fix (\r n -> if n==0 then 10 else add (pot2 n) (r (n-1)))

main = do
  print( add 7 10 )
  print( mult 7 10 )
  print( map pot2 [0..10] )
  print( coc (+1) (*2) 4)
  print( reversR [1..20] )
  print( reversL [1..20] )
  print( map f [0..5] )
