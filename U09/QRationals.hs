 {-# LANGUAGE NPlusKPatterns #-}
{-- Vorlesungsbeispiele. Algebraische Datentypen --}
{-- Author: M. Esponda --}

module QRationals where

data Nat = Zero | S Nat
               deriving Show

add ::  Nat -> Nat -> Nat
add a Zero = a
add a (S b) = add (S a) b

add' ::  Nat -> Nat -> Nat
add' a Zero  = a
add' a (S b) = S (add' a b)

nsucc :: Nat -> Nat
nsucc n = S n

npred :: Nat -> Nat
npred Zero = Zero
npred (S n) = n

mult :: Nat -> Nat -> Nat
mult _ Zero  = Zero
mult a (S b) = add a (mult a b)

{-- power function --}

npow :: Nat -> Nat -> Nat
npow b Zero  = S Zero
npow b (S e) = mult b (npow b e)

factorial :: Nat -> Nat
factorial Zero = S Zero
factorial (S b) = mult (S b) (factorial b)

lt :: Nat -> Nat -> Bool
lt Zero (S _) = True
lt (S a) (S b) = lt a b
lt _ _ = False

gt :: Nat -> Nat -> Bool
gt (S _) Zero = True
gt (S a) (S b) = gt a b
gt _  _  = False

foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero  = c
foldn h c (S n) = h (foldn h c n)

plus :: Nat -> Nat -> Nat
plus = foldn nsucc

multi :: Nat -> Nat -> Nat
multi m = foldn (plus m) Zero

pown :: Nat -> Nat -> Nat
pown m = foldn (mult m) (S Zero)

{-- some naturals for testing --}
one = S Zero
two = S (S Zero)
three = S (S (S Zero))
four  = S (S (S (S Zero)))
five  = S (S (S (S (S Zero))))
six   = S (S (S (S (S (S Zero)))))
seven = S (S (S (S (S (S (S Zero))))))
eight = S (S (S (S (S (S (S (S Zero)))))))
nine  = S (S (S (S (S (S (S (S (S Zero))))))))
ten   = S (S (S (S (S (S (S (S (S (S Zero)))))))))

data ZInt = Z Nat Nat
                 deriving Show

zadd :: ZInt -> ZInt -> ZInt
zadd (Z a b) (Z c d) = Z (add a c) (add b d)

zsub :: ZInt -> ZInt -> ZInt
zsub (Z a b) (Z c d) = Z (add a d) (add b c)

{-- (a,b)*(c,d) equiv. (b-a)*(d-c) = bd - ad - cb + ac
                                   equiv.  (ad + cd, bd + ac)
--}

zmult :: ZInt -> ZInt -> ZInt
zmult (Z a b) (Z c d) = Z (add (mult a d) (mult c b)) (add (mult a c) (mult b d))

zsimplify :: ZInt -> ZInt
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z (S a) (S b)) = zsimplify (Z a b)

zlt :: ZInt -> ZInt -> Bool
zlt (Z a b) (Z c d) = lt (add b c) (add a d)

{-- some positive ZInts for testing --}

zone   =  Z Zero (S Zero)
ztwo   =  Z Zero (S (S Zero))
zthree =  Z Zero (S (S (S Zero)))
zfour  =  Z Zero (S (S (S (S Zero))))
zfive  =  Z Zero (S (S (S (S (S Zero)))))
zsix   =  Z Zero (S (S (S (S (S (S Zero))))))
zseven =  Z Zero (S (S (S (S (S (S (S Zero)))))))
zeight =  Z Zero (S (S (S (S (S (S (S (S Zero))))))))
znine  =  Z Zero (S (S (S (S (S (S (S (S (S Zero)))))))))
zten   =  Z Zero (S (S (S (S (S (S (S (S (S (S Zero))))))))))

{-- some negative ZInts for testing --}

mzone   =  Z (S Zero) Zero
mztwo   =  Z (S (S Zero)) Zero
mzthree =  Z (S (S (S Zero))) Zero
mzfour  =  Z (S (S (S (S Zero)))) Zero
mzfive  =  Z (S (S (S (S (S Zero))))) Zero
mzsix   =  Z (S (S (S (S (S (S Zero)))))) Zero
mzseven =  Z (S (S (S (S (S (S (S Zero))))))) Zero
mzeight =  Z (S (S (S (S (S (S (S (S Zero)))))))) Zero
mznine  =  Z (S (S (S (S (S (S (S (S (S Zero))))))))) Zero
mzten   =  Z (S (S (S (S (S (S (S (S (S (S Zero)))))))))) Zero

data QRational = Q ZInt ZInt
                  deriving Show

{-- (z1,n1) + (z2,n2) equiv. z1/n1 + z2/n2 = (z1*n2 + z2*n1)/(n1*n2)
                      equiv. (z1*n2 + z2*n1, n1*n2)
--}

radd :: QRational -> QRational -> QRational
radd (Q z1 n1) (Q z2 n2) = Q (zadd (zmult z1 n2) (zmult z2 n1)) (zmult n1 n2)

{-- some rational numbers for testing --}

rzero = Q (Z Zero Zero) (Z Zero (S Zero))
rone  = Q (Z Zero (S Zero)) (Z Zero (S Zero))
rtwo  = Q (Z Zero (S (S Zero))) (Z Zero (S Zero))
rthree = Q (Z Zero (S (S (S Zero)))) (Z Zero (S Zero))

--  Natural to Integer

nat2Int :: Nat -> Integer
nat2Int  Zero = 0
nat2Int (S a) = 1 + (nat2Int a)

int2Nat :: Integer -> Nat
int2Nat 0 = Zero
int2Nat (n+1) = (S (int2Nat n))


