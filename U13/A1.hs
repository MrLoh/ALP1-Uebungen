data Expr = App Expr Expr | S | K | I | Var String | Lam String Expr | Nil
                 deriving Eq

instance Show Expr where
  show (App ex1 ex2) = (show ex1)++(show ex2)
  show S = "S "
  show K = "K "
  show I = "I "
  show (Var str) = str++" "
  show (Lam str ex) = str++" "++(show ex)
  show Nil = "NIL "


main = do
  print( App (App S K) (Var "x")  )
