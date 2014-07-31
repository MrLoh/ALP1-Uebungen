data Tree = L | N Tree Tree
              deriving (Show, Eq)

insertLeaf :: Tree -> Tree
insertLeaf L         = (N L L)
--insertLeaf (N L rT)  = (N (N L L) rT)
--insertLeaf (N lT L)  = (N lT (N L L))
insertLeaf (N lT rT) | (depth rT)>=(depth lT) && (full rT) = N (insertLeaf lT) rT
                     | otherwise              = N lT (insertLeaf rT)

insertLeafs :: Tree -> Integer -> Tree
insertLeafs tree 0 = tree
insertLeafs tree n = insertLeafs (insertLeaf tree) (n-1)

deleteLeaf :: Tree -> Tree
deleteLeaf L          = error "can't delete from empty tree"
deleteLeaf (N L L)    = L
deleteLeaf (N lT rT ) | (depth lT)>=(depth rT) = N (deleteLeaf lT) rT
                      | otherwise              = N lT (deleteLeaf rT)

deleteLeafs :: Tree -> Integer -> Tree
deleteLeafs tree 0 = tree
deleteLeafs tree n = deleteLeafs (deleteLeaf tree) (n-1)

depth :: Tree -> Int
depth L         = 0
depth (N lT rT) = 1+(max (depth lT) (depth rT))

full :: Tree -> Bool
full L         = True
full (N lT rT) = (depth lT == depth rT) && (full lT) && (full rT)

main = do
  print( L )
  print( insertLeafs L 1 )
  print( insertLeafs L 2 )
  print( insertLeafs L 3 )
  print( insertLeafs L 4 )
  print( insertLeafs L 5 )
  print( insertLeafs L 6 )
  print( insertLeafs L 7 )
  print( insertLeafs L 10 )
  print( deleteLeaf (insertLeafs L 4) )
  print( deleteLeafs (insertLeafs L 4) 4 )
  print( full (insertLeafs L 4) )
  print( full (insertLeafs L (2^3-1)) )
  print( full (insertLeafs L (2^5-1)) )
