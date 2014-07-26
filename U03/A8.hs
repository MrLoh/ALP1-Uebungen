import Data.Ratio

fac :: Integer -> Integer
fac 0 = 1
fac n | n>0       = n * (fac (n-1))
      | otherwise = error "undefined for negative ints"


binomNaiv :: Integer -> Integer -> Integer
binomNaiv n k | (k<0 || k>n) = error "binom just defined for 0<=k<=n"
              | otherwise    = (fac n) `div` ( (fac k)*(fac (n-k)) )

binomRek :: Integer -> Integer -> Integer
binomRek n k | k>n            = 0
             | k==n || k==0   = 1
             | k==1 || k==n-1 = n
             | otherwise      = (binomRek (n-1) (k-1)) + (binomRek (n-1) k)

binomEff :: Integer -> Integer -> Integer
binomEff n k | k==0      = 1
             | k>0       = let m = (mubi n k) in if denominator m == 1 then numerator m else error""
                           where
                           mubi :: Integer -> Integer -> Rational
                           mubi n 1 = mr n
                           mubi n k = ((mr n - mr k + 1.0) / mr k) * (mubi n (k-1))
                           mr a = toRational a

testBinomFuncs :: Integer -> Integer -> Bool
testBinomFuncs n k = (binomNaiv n k) == (binomRek n k) && (binomRek n k) == (binomEff n k)

main = do
 print( binomNaiv 10 5 )
 print( binomRek 10 5 )
 print( binomEff 10 5 )
 print( testBinomFuncs 10 5 )
