module AST

-- A typical program in our language might look like this:
--
-- main() {
--   var x, y;
--   write "Give me some input, i will calculate the sum";
--   x = read;
--   y = read;
--   write x+y;
-- }
mutual
  public export
  data Statement =  
    Write Expr |
    If Expr Statement (Maybe Statement) |
    Ret Expr |
    Block (List Statement) |
    VarDecl String |
    Ass String Expr

  public export
  data Expr = 
    Add Expr Expr  |
    Sub Expr Expr  |
    Div Expr Expr  |
    Mul Expr Expr  |
    Inp            |
    VarName String |
    StringC String |
    IntC    Int     

public export
record FunDecl where
  constructor MkFunDecl
  name : String
  params : List String
  body : Statement

public export
Program : Type
Program = List FunDecl

