module DataType 

%default total

data Expr : Type where
  Bind : String ->Expr
  Plus : Expr->Expr->Expr
  Cst : Double->Expr
  
expr1 : Expr  
expr1 = Plus (Cst 3) $ Bind "x"

-- f , x , x value
numericDiff : Expr -> String -> Double -> Double
numericDiff (Bind x) y z = 0.0
numericDiff (Plus (Cst a) _) "x" x = (a*(x+0.1)-a*x)/0.1
numericDiff (Cst x) y z = 0.0
numericDiff _ y z = 0.0
  where 
  itv : Double
  itv = 1.0

