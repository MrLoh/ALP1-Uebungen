{-- ALP 1
    SKI parser und eval Functions
    Original version: WS-08-09 Prof. Dr. Raul Rojas
    modified WS-09-10 by: Prof. Dr. Margarita Esponda
    modified WS-13-14 by: Prof. Dr. Margarita Esponda
--}

module SKII (Expr, Show, transform)
       where

-- extended algebraic type for the transform function

data Expr = App Expr Expr | S | K | I | Var String | Lam String Expr | Nil
                 deriving Eq

--instance of Show for Lambda and SKI expressions where show = show_expr

instance Show Expr where show = show_expr

show_expr :: Expr -> String
show_expr exp = show_expr' exp False

show_expr' :: Expr -> Bool -> String
show_expr' S _ = "S"
show_expr' K _ = "K"
show_expr' I _ = "I"

show_expr' (App e1 e2) True = "("++(show_expr' e1 False)++(show_expr' e2 True)++")"
show_expr' (App e1 e2) False= (show_expr' e1 False)++(show_expr' e2 True)

show_expr' (Var x) _ = x

-- returns a list of free variables in an expression

freie :: Expr -> [String] -> [String]

freie S bound = []
freie K bound = []
freie I bound = []
freie (Var x)   bound | elem x bound    = []
                      | otherwise       = [x]
freie (Lam x y) bound = freie y (x:bound)
freie (App x y) bound = (freie x bound) ++ (freie y bound)

----------------------------------------------------------------------

transform S = S
transform K = K
transform I = I
transform (Var x)     = Var x
transform (Lam x exp) = (elim x exp)
transform (App e1 e2)   = App (transform e1) (transform e2)

elim x S  = App K S
elim x K  = App K K
elim x I  = App K I
elim x exp
               | not (elem x (freie exp [])) = (App K (transform exp))

elim x (Var y) | x==y      = I
               | otherwise = (App K (Var y))

elim x (Lam y exp)  = elim x (elim y exp)

elim x (App exp (Var y))
               | not (elem x (freie exp [])) && x==y  = transform exp

elim x (App e1 e2)  = (App (App S (elim x e1)) (elim x e2))

--------------------------------------------------------------------------------

letter x = elem x ['a'..'z']

extract a   []     _  = error "unbalanced parentheses"
extract a (')':b)  0  = (a,b)
extract a (')':b)  n  = extract  (a++")")  b (n-1)
extract a ('(':b)  n  = extract  (a++"(")  b (n+1)
extract a (b:c)    n  = extract  (a++[b])  c  n

-----------------------------------------------------------


zzero = transform (Lam "s" (Lam "z" (Var "z")))
succesor = transform (Lam "w" (Lam "x" (Lam "y" (App (Var "x") (App (App (Var "w") (Var "x")) (Var "y"))) )))
one = transform (Lam "x" (Lam "z" (App (Var "x") (Var "z"))))
two = transform (Lam "x" (Lam "z" (App (Var "x") (App (Var "x") (Var "z")))))
three = transform (App succesor two)
four = transform (App succesor three)
five = transform (App succesor four)
six = transform (App succesor five)
seven = transform (App succesor six)
eight = transform (App succesor seven)

exp1 = transform          (Lam "x"      (App (Var "y") (App (Var "x") (Var "y"))) ) -- (/x.y(xy))
exp2 = transform (Lam "z" (Lam "x" (App (App (Var "y") (App (Var "x") (Var "y"))) (Var "z")) )) --(/zx.y(xy)z)

testnum = transform (Lam "x" (App (App (Var "x") (Var "s")) (Var "z")))

mult = transform (Lam "x" (Lam "y" (Lam "z" (App (Var "x") (App (Var "y") (Var "z"))) )))

true  = transform (Lam "a" (Lam "b" (Var "a")))
false = transform (Lam "a" (Lam "b" (Var "b")))
and2  = transform (Lam "x" (Lam "y" (App (App (Var "x") (Var "y")) false) ))
or2   = transform (Lam "x" (Lam "y" (App (App (Var "x")    true) (Var "y")) ))
not2  = transform (Lam "x" (App (App (Var "x") false) true) )

test_zero = transform (Lam "x" (App(App(App (Var "x") false) not2) false))

par_00 = transform (Lam "z" (App(App (Var "z") zzero) zzero))
phi    = transform (Lam "p" (Lam "z"
    (App ( App (Var "z") (App succesor (App (Var "p") true))) (App (Var "p") true)  )))
prede = transform (Lam "n" (App (App (App (Var "n") phi) par_00) false))

greater_eq = transform (Lam "x" (Lam "y" (App test_zero (App (App (Var "x") prede) (Var "y"))) ))

rec = transform (Lam "y" (App (Lam "x" (App (Var "y") (App (Var "x") (Var "x"))))
                    (Lam "x" (App (Var "y") (App (Var "x") (Var "x"))))))

summe = transform (Lam "r" (Lam "n"
      (App (App (App test_zero (Var "n")) zzero)
      (App (App (Var "n") succesor) (App (Var "r") (App prede (Var "n"))))
      )))

fakul = transform (Lam "r" (Lam "n"
       (App (App (App test_zero (Var "n")) one)
       (App (App mult (Var "n")) (App (Var "r") (App prede (Var "n"))))
       )))

simple = transform (Lam "x" (Lam "y" (App (Var "x") (Var "x"))))

simple2 = transform (Lam "x" (Lam "y" (App (Var "x") (Var "y"))))

simple3 = transform (Lam "x" (Lam "y" (App (Var "y") (Var "x"))))

xc = transform (Lam "f" (App (App (App (Var "f") K) S) K))

main = do
  print( zzero )
