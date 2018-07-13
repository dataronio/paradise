module Elabs
import Language.Reflection.Elab

%language ElabReflection
-- https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Language/Reflection/Elab.idr
  
addTypeDecl : Elab ()
addTypeDecl = do 
  let tname : TTName = `{{GenData}}
  declareDatatype $ Declare tname []  RType
  defineDatatype $ DefineDatatype tname []
  
%runElab addTypeDecl

genTypeTest : GenData -> ()
genTypeTest x = ()

--  generate lambdas
idNat : Nat -> Nat
idNat = %runElab (do intro `{{x}}
                     fill (Var `{{x}})
                     solve)
