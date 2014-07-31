import Data.Maybe
--implementation with Moore's voting algorythm
majority :: (Eq a) => [a] -> (Maybe a)
majority [] = Nothing
majority xs = if (length xs)`div`2 < candidateCount
              then Just candidate
              else Nothing
              where
              candidate = findCandidate (tail xs) (head xs) 1
              candidateCount = count xs candidate
              --finds the most occuring element of a list in O(n)
              findCandidate :: (Eq a) => [a] -> a -> Int -> a
              findCandidate []     a count = a
              findCandidate (x:xs) a count | x==a          = findCandidate xs a (count+1)
                                           | (count-1) > 0 = findCandidate xs a (count-1)
                                           | otherwise     = findCandidate xs x 1
              --counts the occurence of a key in a list in O(n)
              count :: (Eq a) => [a] -> a -> Int
              count [] a     = 0
              count (x:xs) a | x==a      = 1+(count xs a)
                             | otherwise = (count xs a)

main = do
 print( majority [1,1,1,0,2] )
 print( majority [1,1,1,0,2,4] )
