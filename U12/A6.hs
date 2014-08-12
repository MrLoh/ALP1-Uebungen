------------------------------from U11 A6-----------------------------------------
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
           matchBrackets ((x:xs),i) | x`elem`[')',' '] && i>0 = matchBrackets (xs,(i-1))
                                    | otherwise     = False
           matchBrackets (_,_)      = False
----------------------------------------------------------------------------------




int2lambda :: Integer -> String
int2lambda n | n>=0      = "/z.z ("++(nat2lambda n)++") ("++(nat2lambda 0)++")"
             | otherwise = "/z.z ("++(nat2lambda 0)++") ("++(nat2lambda (-n))++")"

--lambda2int :: String -> Integer
--lambda2int (l:z1:p:z2:nat)
--           | corrStart = (lambda2nat (fst nats)) - (lambda2nat (snd nats))
--           | otherwise = syntaxError
--           where
--           syntaxError = error "invalid Syntax for Lambda Expression"
--           corrStart :: Bool
--           corrStart = l=='/' && z1==z2 && p=='.'
--           nats :: (String, String)
--           nats = (matchNats nat)
--           matchNats :: String -> (String,String)
--           matchNats (b:l:xs) (n,m) | b=='(' && l=='/' = getNat xs ()
--                                    | otherwise        = matchNats (l:xs)
--           matchNats _              = syntaxErrorb


main = do
  print( int2lambda 5 )
  print( int2lambda (-2) )
  --print( lambda2int "/z.z (/x./y.x(x(x(y)))) (/x.y.y)")

