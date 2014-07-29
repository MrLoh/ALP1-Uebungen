flattenL :: [[a]] -> [a]
flattenL ls = foldl (++) [] ls

flattenR :: [[a]] -> [a]
flattenR ls = foldr (++) [] ls

main = do
 print( flattenL ["1", "2", "3"] )
 print( flattenR ["1", "2", "3"] )
 print( flattenR ["",""] )
