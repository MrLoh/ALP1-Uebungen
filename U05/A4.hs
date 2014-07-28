import Data.List

listOfSuffixes :: [a] -> [[a]] --O(n)
listOfSuffixes []     = []
listOfSuffixes (x:xs) = (x:xs) : listOfSuffixes xs

prefix :: (Eq a) => [a] -> [a] -> [a] --O(n)
prefix (x:xs) (y:ys) | x==y      = x:(prefix xs ys)
                     | otherwise = []
prefix _      _      = []

longestPrefix :: (Eq a) => [[a]] -> (Int,[a]) --O(n^2)
longestPrefix ls = lgsPf ls (0,[]) --n-times lgsPf
                   where
                   lgsPf :: (Eq a) => [[a]] -> (Int,[a]) -> (Int,[a])
                   lgsPf (l1:l2:ls) curr | lpf > fst curr = lgsPf (l2:ls) (lpf,pf) --O(n)
                                         | otherwise      = lgsPf (l2:ls) curr     --O(n)
                                           where
                                           lpf = length pf   --O(n)
                                           pf = prefix l1 l2 --O(n)
                   lgsPf _          curr = curr


longestRepSeq :: (Ord a) => [a] -> [a] --O(n^2)
longestRepSeq = snd.longestPrefix.sort.listOfSuffixes

main = do
 print( listOfSuffixes "xyzab" )
 print( prefix "abcde" "abacde" )
 print( longestPrefix ["a","abca","bca","bcadabca","ca","cdabca"] )
 print( longestRepSeq "absdadsdahko" )
 print( longestRepSeq "aabcfdesababcdferfcsdeaedabcfdesabeda" )
 print( longestRepSeq [1,1,0,1,1,0,0,1,0,1,1] )
