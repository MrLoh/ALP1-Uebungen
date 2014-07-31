--foldr :: (a->b->b) -> b -> [a] -> b --linear rekursiv
--foldr f z []     = z
--foldr f z (x:xs) = f x (foldr f z xs)

--foldl :: (a->b->a) -> a -> [b] -> a --endrekursiv
--foldl f z []     = z
--foldl f z (x:xs) = foldl f (f z x) xs

flattenL :: [[a]] -> [a]
flattenL ls = foldl (++) [] ls

flattenR :: [[a]] -> [a]
flattenR ls = foldr (++) [] ls

main = do
 print( flattenL ["1", "2", "3"] )
 print( flattenR ["1", "2", "3"] )
 print( flattenR ["",""] )

{-
flattenR ["1","2","3"]
=> foldr (++) [] ["1","2","3"]
=> (++) "1" (foldr (++) [] ["2","3"])
=> (++) "1" ( (++) "2" (foldr (++) [] ["3"]) )
=> (++) "1" ( (++) "2" ( (++) "3" (foldr (++) [] []) ) )
=> (++) "1" ( (++) "2" ( (++) "3" [] ) )
=> (++) "1" ( (++) "2" "3" )
=> (++) "1" "23"
=> "123"
-}

{-
flattenL ["1","2","3"]
=> foldl (++) [] ["1","2","3"]
=> foldl (++) ( (++) [] "1" ) ["2","3"]
=> foldl (++) ( (++) ( (++) [] "1" ) "2" ) ["3"]
=> foldl (++) ( (++) ( (++) ( (++) [] "1" ) "2" ) "3" ) []
=> (++) ( (++) ( (++) [] "1" ) "2" ) "3" )
=> (++) ( (++) "1" "2" ) "3"
=> (++) "12" "3"
=> "123"

wegen (++) ist hier flattenL ineffektiver, weil es zunächst die linke Listt wachsen lässt.
-}
