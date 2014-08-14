import Data.List

-- 1
isin :: (Eq a) => a -> [a] -> Bool --O(n)
isin k [] = False
isin k (x:xs)
     | k==x      = True
     | otherwise = isin k xs

findapbgc :: (Num a,Ord a) => [a] -> Maybe (a,a,a)
findapbgc xs = aux (sort xs) -- sort: O(n*log n)
               where
               aux [] = Nothing
               aux (x:xs)
                   | fst res   = Just (snd res)
                   | otherwise = aux xs
                   where
                   res = test x xs
                   test :: (Num a,Ord a) => a -> [a] -> (Bool,(a,a,a)) -- O(n^2)
                   test k [] = (False,(0,0,0))
                   test k (x:xs)
                        | isin (k+x) xs = (True,(k,x,k+x)) --O(n)
                        | otherwise     = test k xs
{-
1. findapbgc sortiert die Liste in O(n*log n)
2. danach geht aux die Liste im schlimmsten Fall einmal durch
3. dabei ruft aux jedesmal test auf, welches im schlimmsten Fall die ganze Liste urchgeht
4. dabei ruft test jedesmal isin auf, mit O(n)
=> O( n*log n + (n*n*n) ) = O(n^3)
-}


-- 2
data Ausdruck = Konst Int | Var String | Plus Ausdruck Ausdruck | Mal Ausdruck Ausdruck

tiefeKonst :: Ausdruck -> Int
tiefeKonst (Konst n)    = 1
tiefeKonst (Var str)    = 1
tiefeKonst (Plus a1 a2) = 1 + max (tiefeKonst a1) (tiefeKonst a2)
tiefeKonst (Mal a1 a2)  = 1 + max (tiefeKonst a1) (tiefeKonst a2)


-- 3
rev :: [a] -> [a] -- O(n)
rev = foldl (\z x -> x:z) []

vielfachheit :: [Int] -> [Int]
vielfachheit xs = rev (aux (sort xs) 0 True []) -- rev: O(n), sort: O(n*log n)
                  where
                  aux :: [Int] -> Int -> Bool -> [Int] -> [Int]
                  aux []     i iEmpty akk = akk
                  aux (x:xs) i iEmpty akk
                      | iEmpty    = if i==x
                                    then aux xs i False (1:akk)
                                    else aux (x:xs) (i+1) True (0:akk)
                      | otherwise = if i==x
                                    then aux xs i False (1+(head akk):tail akk)
                                    else aux (x:xs) (i+1) True akk
{-
1. vielfachheit ruft zunächst sort mit O(n*log n) auf
2. danach wird aux aufgerufen, dies geschiet m (= maximum xs) mal.
3. aux hat in jedem Schritt nur einfache Operationen
4. am Ende wird rev mit O(m) aufgerufen
=> O(n*log n + m + m) = O(n*log n + m)
-}


-- 4
int2coins :: Int -> [Int]
int2coins n = aux n [] coins
              where
              coins = [300,100,53,23,13,7,2,1]
              aux :: Int -> [Int] -> [Int] -> [Int]
              aux 0 akk cs = akk
              aux n akk (c:cs)
                  | n>=c      = aux (n-c) (c:akk) (c:cs)
                  | otherwise = aux n akk cs
-- passt nicht immer ganze, zB: 106 -> [53,53] funktioniert nicht


-- 5
data ST a = N | H a (ST a) deriving(Show, Eq)

leer :: ST a
leer = N

push :: (ST a,a) -> ST a
push (N, a) = H a N
push (st, a) = H a st

top :: ST a -> a
top N        = error "Empty Stack"
top (H x st) = x

pop :: ST a -> ST a
pop N        = error "Empty Stack"
pop (H x st) = st

istLeer :: ST a -> Bool
istLeer N = True
istLeer _ = False


-- 6
data BSTree a = Nil | Node a (BSTree a) (BSTree a) deriving(Show, Eq)

inser :: (Ord a) => a -> BSTree a -> BSTree a
inser k Nil = Node k Nil Nil
inser k (Node x lT rT)
      | k<x  = Node x (inser k lT) rT
      | k>x  = Node x lT (inser k rT)
      | k==x = error "No duplicates allowed in BSTree"

search :: (Ord a) => a -> BSTree a -> Bool
search k Nil = False
search k (Node x lT rT)
       | k==x = True
       | k<x  = search k lT
       | k>x  = search k rT


main = do
  print( findapbgc [24,9,2,4,7,3] )
  print( findapbgc [50..100] )
  putStrLn ""
  print( vielfachheit [1,3,5,3,1] )
  putStrLn ""
  print( int2coins 65 )
  print( int2coins 106 )
  putStrLn ""
  print( push (push (leer, 1),2) )
  print( top (push (push (leer, 1),2)) )
  print( pop (push (push (leer, 1),2)) )
  print( istLeer leer )
  putStrLn ""
  print( inser 5 Nil )
  print( inser 7 (inser 3 (inser 5 Nil)) )
  print( search 7 (inser 7 (inser 3 (inser 5 Nil))) )
  print( search 3 (inser 7 (inser 3 (inser 5 Nil))) )
  print( search 8 (inser 7 (inser 3 (inser 5 Nil))) )

