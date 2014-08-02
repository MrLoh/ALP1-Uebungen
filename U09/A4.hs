int2decList :: Integer -> [Int] --O(log z)
int2decList z | z<10      = [fromEnum z]
              | otherwise = (int2decList (z`div`10))++[fromEnum z`mod`10]

makeSubLists :: Int -> [a] -> [[a]] --O(len xs)
makeSubLists k xs | length xs<k = [] --hierdurch => k<len xs
                  | otherwise   = (take k xs):(makeSubLists k (tail xs)) --O(k*len xs)
                  -- length: O(len xs), take k: O(k), tail: O(1)

prod :: [Int] -> Int --O(len xs)
prod xs = foldl (*) 1 xs

maxProdNeihgbours :: Integer -> Int -> Int --O(log z)
maxProdNeihgbours z k
                  | zkLis==[] = error "k larger than z long"
                  | otherwise = maximum (map prod zkLis) --sequentiell, also O(log z)
                  --map: O(len xs), maximum: O(len xs)
                  where
                  zkLis = makeSubLists k (int2decList z) --sequentiell, also O(log z)
                  --len (int2decList z) = log z

{-
Alle ausführungen sind sequentiell, also ist die Komplexistät durch die Länge der Dezimaldarstellung von z, welche log_10 z beträgt gegeben.
-}

main = do
  print( int2decList 61905 )
  print( maxProdNeigbours 64905 1 )
  print( maxProdNeigbours 64905 2 )
  print( maxProdNeigbours 64905 3 )
  print( maxProdNeigbours 64905 4 )
  print( maxProdNeigbours 64905 6 )
