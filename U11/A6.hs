nat2lambda :: Integer -> String
nat2lambda i = "/x./y." ++ helper i
               where
               helper :: Integer -> String
               helper 0 = "y"
               helper i = "x(" ++ (helper (i-1)) ++ ")"

lambda2nat :: String -> Integer
lambda2nat (l1:s:p1:l2:z:p2:xs)
           | corrStart = if matchBrackets (helper xs 0)
                         then snd (helper xs 0)
                         else syntaxError
           | otherwise = syntaxError
           where
           syntaxError = error "invalid Syntax for Lambda Expression"
           corrStart :: Bool
           corrStart = l1=='/' && l2=='/' && p1=='.' && p2=='.' && s`elem`letters && z`elem`letters
           letters :: [Char]
           letters = "abcdefghijklmnopqrstuvwxyz"
           helper :: String -> Integer -> (String, Integer)
           helper xs i | (head xs)==z = (tail xs, i)
                       | (head xs)==s = if (head (tail xs))=='('
                                        then helper (drop 2 xs) (i+1)
                                        else syntaxError
                       | otherwise    = syntaxError
           matchBrackets :: (String, Integer) -> Bool
           matchBrackets ([],0)     = True
           matchBrackets ((x:xs),i) | x==')' && i>0 = matchBrackets (xs,(i-1))
                                    | otherwise     = False
           matchBrackets (_,_)      = False

main = do
  print( nat2lambda 3 )
  print( nat2lambda 0 )
  print( lambda2nat "/x./y.x(x(x(y)))" )
  print( lambda2nat "/s./z.z" )
  print( lambda2nat "/x./y.x(x(x(y))" )
