data Tree = L | N Tree Tree
              deriving Show

insertLeaf :: Tree -> Tree
insertLeaf L         = (N L L)
insertLeaf (N lT rT) | (depth lT)<=(depth rT) = N (insertLeaf lT) rT
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

main = do
 print( L )
 print( insertLeafs L 10 )
 print( deleteLeaf (insertLeafs L 4) )
 print( deleteLeafs (insertLeafs L 4) 4 )
 print( deleteLeafs (insertLeafs L 4) 5 )
