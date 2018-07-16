module Frees

import Control.Monad.Freer
import Debug.Trace
import Control.Monad.State

data Tensor : Type where
  DoubleT : Double -> Tensor

implementation Show Tensor where
  show (DoubleT x) = show x

data GraphData : Type -> Type where 
  Mul : Tensor -> Tensor -> GraphData Tensor
  Placeholder : GraphData Tensor
  Constant : Double -> GraphData Tensor

FreeGraph : Type --type of computation graph ,freer graph
FreeGraph = Freer GraphData Tensor

mulG : FreeGraph
mulG = do
  x<-liftF $ Constant 2
  y<-liftF $ Constant 3
  liftF $ Mul x y

st : State String Int
st = ST (\str => Id (1, str++"1,"))

stm : State String Int
stm = do
  st
  st

free2state : {x : Type} -> GraphData x -> State String x
free2state (Mul (DoubleT x) (DoubleT y)) = ST (\str => Id (DoubleT $ x*y, str++"mul"))
free2state Placeholder = ST (\str => Id (DoubleT $ 0.0, str++"Placeholder,"))
free2state (Constant x) = ST (\str => Id (DoubleT $ x, str++"Constant,"))

f2st : State String Tensor
f2st = foldFreer free2state mulG
