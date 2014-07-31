{-# LANGUAGE InstanceSigs #-}

data Nat = Zero | S Nat
           deriving Show

nat2int :: Nat -> Integer
nat2int Zero  = 0
nat2int (S n) = 1 + (nat2int n)

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat i = (S (int2nat (i-1)))

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Zero  Zero  = True
  (==) (S n) (S m) = (==) n m
  (==) _     _     = False

  (/=) :: Nat -> Nat -> Bool
  (/=) n m = not (n==m)

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) n Zero  = n
  (+) n (S m) = (+) (S n) m

  (-) :: Nat -> Nat -> Nat
  (-) Zero  m     = Zero
  (-) n     Zero  = n
  (-) (S n) (S m) = (-) n m

  (*) :: Nat -> Nat -> Nat
  (*) n Zero  = Zero
  (*) n (S m) = n + ((*) n m)

  negate n = n
  abs n = n
  signum n = (S Zero)
  fromInteger = int2nat

npow :: Nat -> Nat -> Nat
npow _ Zero  = (S Zero)
npow n (S m) = n * (npow n m)

nsucc :: Nat -> Nat
nsucc n = (S n)

npred :: Nat -> Nat
npred Zero = Zero
npred (S n) = n

instance Ord Nat where
  (<) :: Nat -> Nat -> Bool
  (<) Zero  (S _) = True
  (<) (S n) (S m) = (<) n m
  (<) _     _     = False

  (<=) :: Nat -> Nat -> Bool
  (<=) n m = n<m || n==m

  (>) :: Nat -> Nat -> Bool
  (>) (S _) Zero  = True
  (>) (S n) (S m) = (>) n m
  (>) _     _     = False

  (>=) :: Nat -> Nat -> Bool
  (>=) n m = n>m || n==m

  compare :: Nat -> Nat -> Ordering
  compare n m | n<m  = LT
              | n==m = EQ
              | n>m  = GT

  min :: Nat -> Nat -> Nat
  min n m = n-(n-m)

  max :: Nat -> Nat -> Nat
  max n m = m+(n-m)

neven :: Nat -> Bool
neven Zero      = True
neven (S Zero)  = False
neven (S (S n)) = neven n

nuneven :: Nat -> Bool
nuneven n = not (neven n)


integral :: Nat -> Nat -> Nat -> (Nat, Nat)
integral n Zero c = error "no division by Zero"
integral n m c | n<m       = (c, n)
               | otherwise = integral (n-m) m (c+(S Zero))

instance Real Nat where
instance Enum Nat where
instance Integral Nat where
  divMod :: Nat -> Nat -> (Nat,Nat)
  divMod n m = integral n m Zero

  div :: Nat -> Nat -> Nat
  div n m = fst (divMod n m)

  mod :: Nat -> Nat -> Nat
  mod n m = snd (divMod n m)

nMaxDiv :: Nat -> Nat -> Nat
nMaxDiv n m = helper n m (min n m)
              where
              helper :: Nat -> Nat -> Nat -> Nat
              helper n m Zero     =  error "No maxdiv of Zero"
              helper n m c | n`mod`c==Zero && m`mod`c==Zero = c
                           | otherwise = helper n m (c-(S Zero))










data Zahl = Z Nat Nat
            deriving Show

zsimplify :: Zahl -> Zahl
zsimplify (Z n1 n2) = Z (n1-n2) (n2-n1) --works because flooring at Zero for Nats

zahl2int :: Zahl -> Integer
zahl2int z | n1/=Zero = -(nat2int n1)
           | otherwise   = (nat2int n2)
           where
           (Z n1 n2) = zsimplify z

int2zahl :: Integer -> Zahl
int2zahl i | i<0       = (Z (int2nat i) Zero)
           | otherwise = (Z Zero (int2nat i))

instance Eq Zahl where
  (==) :: Zahl -> Zahl -> Bool
  (==) z1 z2 | n1==m1 && n2==m2 = True
             | otherwise        = False
             where
             (Z n1 n2) = zsimplify z1
             (Z m1 m2) = zsimplify z2

  (/=) :: Zahl -> Zahl -> Bool
  (/=) z1 z2 = not (z1==z2)

instance Num Zahl where
  (+) :: Zahl -> Zahl -> Zahl
  (+) (Z n1 n2) (Z m1 m2) = Z (n1+m1) (n2+m2)

  (-) :: Zahl -> Zahl -> Zahl
  (-) (Z n1 n2) (Z m1 m2) = Z (n1+m2) (n2+m1)

  (*) :: Zahl -> Zahl -> Zahl
  (*) (Z n1 n2) (Z m1 m2) = Z (m1*n2+m2*n1) (m1*n1+m2*n2)

  negate :: Zahl -> Zahl
  negate (Z n1 n2) = (Z n2 n1)

  abs :: Zahl -> Zahl
  abs z | n1==Zero  = (Z n2 Zero)
        | otherwise = (Z n1 Zero)
        where
        (Z n1 n2) = zsimplify z

  signum :: Zahl -> Zahl
  signum z | n1==Zero  = (Z Zero (S Zero))
           | otherwise = (Z (S Zero) Zero)
           where
           (Z n1 n2) = zsimplify z

  fromInteger = int2zahl

zpow :: Zahl -> Nat -> Zahl
zpow z m | n1/=Zero = (Z (n1`npow`m) Zero)
         | otherwise   = (Z Zero (n2`npow`m))
         where
         (Z n1 n2) = zsimplify z

zMaxDiv :: Zahl -> Zahl -> Zahl
zMaxDiv z1 z2 = Z Zero (nMaxDiv n m)
                where
                (Z n Zero) = abs z1
                (Z m Zero) = abs z2

main = do
 print( nat2int (S (S Zero)) )
 print( int2nat 5 )
 print( nat2int (int2nat 3 + int2nat 4) )
 print( nat2int (int2nat 10 + int2nat 4) )
 print( nuneven (int2nat 3) )
 print( nuneven (int2nat 10) )
 print( neven (int2nat 10) )
 print( nat2int (min (int2nat 10) (int2nat 11)) )
 print( nat2int (max (int2nat 10) (int2nat 11)) )
 print( neven (int2nat 9) )
 print( nat2int ((int2nat 6)`div`(int2nat 3)) )
 print( nat2int ((int2nat 6)`mod`(int2nat 3)) )
 print( nat2int (nMaxDiv (int2nat 12) (int2nat 6)) )
 print( zahl2int (zMaxDiv (int2zahl 12) (int2zahl 6)) )
