data ABaum a = Nil | Node a [ABaum a]
              deriving (Show, Eq)

children :: ABaum a -> Int
children Nil         = 0
children (Node a bs) = 1+(sum(map children bs))

depth :: ABaum a -> Int
depth Nil         = 0
depth (Node a bs) = 1+(maximum (map depth bs))

find :: (Eq a) => a -> ABaum a -> Bool
find k Nil = False
find k (Node a bs)
     | k==a      = True
     | otherwise = or (map (find k) bs)

mapTree :: (a -> a) -> ABaum a -> ABaum a
mapTree f Nil         = Nil
mapTree f (Node a bs) = Node (f a) (map (mapTree f) bs)


main = do
  print( baum )
  print( children baum )
  print( depth baum )
  print( find 4 baum )
  print( find 10 baum )
  print( mapTree (+1) baum )
  where
  baum = Node 1 [Node 2 [Node 4 [Nil], Node 5 [Nil]], Node 3 [Node 6 [Nil], Node 7 [Nil]] ]
