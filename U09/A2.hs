import QRationals

qreciprocal :: QRational -> QRational
qreciprocal (Q z n) = Q n z

qadd :: QRational -> QRational -> QRational
qadd (Q z1 n1) (Q z2 n2) = Q (zadd (zmult z1 n2) (zmult z2 n1)) (zmult n1 n2)

qmult :: QRational -> QRational -> QRational
qmult (Q z1 n1) (Q z2 n2) = Q (zmult z1 z2) (zmult n1 n2)

qquotient :: QRational -> QRational -> QRational
qquotient q1 q2 = qmult q1 (qreciprocal q2)

main = do
  print( qadd rone rtwo )
  print( qmult rone rtwo )
  print( qquotient rone rtwo )
  print( rtwo )
  print( qreciprocal rtwo )
