import Data.List
type Set = [Int]

settify :: [Int] -> Set
settify = delDupes.sort

delDupes :: [Int] -> [Int]
delDupes []       = []
delDupes [x]      = [x]
delDupes (x:y:xs) | x==y      = delDupes (y:xs)
                  | otherwise = x:(delDupes (y:xs))

--we will further assume, that sets have no double elements and are sorted

--Element
el :: Int -> Set -> Bool
el a [] = False
el a (e:set) | a==e      = True
             | a<e       = False
             | otherwise = el a set

--Vereinigungsmenge
u :: Set -> Set -> Set
u set1 set2 = settify (set1 ++ set2)

--Mengendifferenz
d :: Set -> Set -> Set
d set1 set2 = [ e | e <- set1 , not(e`el`set2) ]

--Schnittmenge
n :: Set -> Set -> Set
n set1 set2 = [ e | e <- set1 , e`el`set2 ]

--Symmetrische Differenz
x :: Set -> Set -> Set
x set1 set2 = (set1 `u` set2) `d` (set1 `n` set2 )

--Untermenge
sub :: Set -> Set -> Bool
sub set1 set2 = (set1 `n` set2 == set1)

--Mengengleichheit
eq :: Set -> Set -> Bool
eq set1 set2 = set1 `sub` set2 && set2 `sub` set1


main = do
 print( settify [4,1,2,3,1] )
 putStrLn ""
 print( [1,2,3]`u`[2,3,4,5] )
 print( [2,3,4,5]`u`[1,2,3] )
 print( [1,2,3]`n`[2,3,4,5] )
 print( [2,3,4,5]`n`[1,2,3] )
 print( [1,2,3]`d`[2,3,4,5] )
 print( [2,3,4,5]`d`[1,2,3] )
 print( [1,2,3]`x`[2,3,4,5] )
 print( [2,3,4,5]`x`[1,2,3] )
 print( [1,2,3]`sub`[1,2,3,4] )
 print( [2,3,4,5]`sub`[1,2,3] )
 print( [1,2,3]`eq`[1,2,3] )
 print( [1,2,3]`eq`[1,2,3,4] )


