module DataType 

%default total

data Expr : Type where
  Bind : String ->Expr
  Plus : Expr->Expr->Expr
  Cst : Double->Expr -- constant
  
expr1 : Expr  
expr1 = Plus (Cst 3) $ Bind "x"

expr2 : Expr  
expr2 = Plus (Cst 2) $ Plus (Cst 3) $ Bind "x"
 
numericDf : (Double->Double) -> Double ->Double
numericDf f x = ((f $ x+0.1) - f x )/0.1

--expression,varname
exp2f : Expr->String -> (Double->Double)
exp2f (Bind x) y = if (x==y) then (\x=>x) else (\_=>0.0)
exp2f (Plus x z) y = (\arg=>((exp2f x y) arg)*((exp2f z y) arg))
exp2f (Cst x) y = \_=>x

-- f , x , x value
numericDiffExpr : Expr -> String -> Double -> Double
numericDiffExpr x y z = numericDf (exp2f x y) z


-- data W : (A : Type)->(B : A -> Type)->Type where
--  sup : {A : Type}->{B : A -> Type}->(a : A)-> (y : {A : Type}->(x : A)->B x -> W x ) -> W x y
-- https://github.com/agda/agda-stdlib/blob/master/src/Data/W.agda
data W : (A : Type)-> (B : A -> Type) -> Type where
  sup : {A : Type}-> {B : A -> Type}->(x : A) -> (f : B x -> W A B) -> W A B
