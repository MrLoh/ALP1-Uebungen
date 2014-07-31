{-# LANGUAGE InstanceSigs #-}

data SqRootNum = SRN Int Int
                 deriving Show

instance Num SqRootNum where
  (+) :: SqRootNum -> SqRootNum -> SqRootNum
  (+) (SRN a1 b1) (SRN a2 b2) = SRN (a1+a2) (b1+b2)

  (-) :: SqRootNum -> SqRootNum -> SqRootNum
  (-) (SRN a1 b1) (SRN a2 b2) = SRN (a1-a2) (b1-b2)

  (*) :: SqRootNum -> SqRootNum -> SqRootNum
  (*) (SRN a1 b1) (SRN a2 b2) = SRN (a1*a2+2*b1*b2) (a1*b2+b1*a2)

  abs :: SqRootNum -> SqRootNum
  abs (SRN a b) = (SRN (abs a) (abs b) )

  signum :: SqRootNum -> SqRootNum
  signum (SRN a b) = SRN (signum (fromEnum val)) 0
                     where
                     val = (fromIntegral a)+(fromIntegral b)*sqrt(2)

  fromInteger :: Integer -> SqRootNum
  fromInteger i = SRN (fromEnum i) 0

main = do
  print( (SRN 3 2) + (SRN 2 1) )
  print( (SRN 3 2) - (SRN 2 1) )
  print( (SRN 3 2) * (SRN 2 1) )
