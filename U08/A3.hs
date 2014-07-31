data BSTree a = Nil | N a (BSTree a) (BSTree a)
                     deriving ( Show, Eq )

minA :: (Ord a) => BSTree a -> a
minA (N a Nil _) = a
minA (N a leT _) = minA leT

maxA :: (Ord a) => BSTree a -> a
maxA (N a _ Nil) = a
maxA (N a _ riT) = maxA riT

inOrder :: (Ord a) => BSTree a -> [a]
inOrder Nil           = []
inOrder (N x leT riT) = (inOrder leT) ++ [x] ++ (inOrder riT)

preOrder :: (Ord a) => BSTree a -> [a]
preOrder Nil           = []
preOrder (N x leT riT) = [x] ++ (preOrder leT) ++ (preOrder riT)

postOrder :: (Ord a) => BSTree a -> [a]
postOrder Nil           = []
postOrder (N x leT riT) = (postOrder leT) ++ (postOrder riT) ++ [x]

height :: BSTree a -> Int
height Nil           = 0
height (N a leT riT) = 1+(max (height leT) (height riT))

balanced :: BSTree a -> Bool
balanced Nil = True
balanced (N a leT riT) = (balanced leT) && (balanced riT)
                         && (height leT == height riT)

twoChildren :: (Ord a) => BSTree a -> Bool
twoChildren Nil                     = True
twoChildren (N a Nil (N b leT riT)) = False
twoChildren (N a (N b leT riT) Nil) = False
twoChildren (N a leT riT) = (twoChildren leT) && (twoChildren riT)




search :: (Ord a) => a -> BSTree a -> Bool
search k Nil = False
search k (N a leT riT) | k<a  = search k leT
                       | k==a = True
                       | k>a  = search k riT

insert :: (Ord a) => a -> BSTree a -> BSTree a
insert k Nil           = N k Nil Nil
insert k (N a leT riT) | k<a       = N a (insert k leT) riT
                       | otherwise = N a leT (insert k riT)

delete :: (Ord a) => a -> BSTree a-> BSTree a
delete k Nil           = Nil
delete k (N a leT riT) | k<a  = N a (delete k leT) riT
                       | k==a = join leT riT
                       | k>a  = N a leT (delete k riT)

join :: (Ord a) => BSTree a -> BSTree a -> BSTree a
join leT Nil = leT
join leT riT = N n leT nT
                where
                (n, nT) = splitMin riT
                splitMin :: (Ord a) => BSTree a -> (a, BSTree a)
                splitMin (N a Nil riT) = (a, riT)
                splitMin (N a leT riT) = (n, (N a nT riT))
                                         where
                                         (n, nT) = splitMin leT

list2Tree :: (Ord a) => [a] -> BSTree a
list2Tree []     = Nil
list2Tree (a:as) = insert a (list2Tree as)




mapTree :: (Ord a, Ord b) => (a -> b) -> BSTree a -> BSTree b
mapTree op tree = list2Tree (map op (preOrder tree))

foldTree :: (Ord a) => b -> (a -> b -> b) -> BSTree a -> b
foldTree s op tree = foldr op s (preOrder tree)


main = do
  print( tree )
  print( preOrder tree )
  print( minA tree )
  print( maxA tree )
  print( height tree )
  print( balanced tree )
  print( twoChildren tree )
  print( balanced balTree )
  print( twoChildren balTree )
  print( inOrder tree )
  print( inOrder (delete 27 tree) )
  where
  tree = list2Tree [53,27,69,13,34,63,95,17,46]
  balTree = ( N 3 (N 1 Nil Nil) (N 4 Nil Nil) )
