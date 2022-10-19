module DumbCC.Parser.Types.Sugared where

data ExprS
  = ELit Lit
  | EId String
  | EUn UOp ExprS
  | EBi BOp ExprS ExprS
  | EApp ExprS ExprS
  | EIdx ExprS ExprS

data Prog s
  = Prog [Func s]

data Func s = Func
  { fName :: String,
    fRet :: String,
    fArgs :: [(String, String)],
    fBody :: s
  }

-- | A sugared statement
data SStmt
  = SScope [SStmt]
  | SBreak
  | SSingleExpr ExprS
  | SAsgn String ExprS
  | SDecl String String ExprS
  | SIf ExprS SStmt SStmt
  | SFor SStmt ExprS SStmt SStmt
  | SWhile ExprS SStmt

-- | A desugared statement
data CStmt
  = CScope [CStmt]
  | CBreak
  | CSingleExpr ExprS
  | CAsgn String ExprS
  | CDecl String String ExprS
  | CIf ExprS SStmt SStmt
  | CWhile ExprS SStmt

data Lit = LStr String | LNum String

data UOp
  = Inc
  | Dec
  | Neg
  | Pos
  | Deref
  | Ref

data BOp
  = Add
  | Sub
  | Mul
  | Div
  | BAnd
  | BOr
  | BNot
  | LAnd
  | LOr
  | LNot
  | Xor
  | Eq
  | Neq
  | Ge
  | Le
  | Geq
  | Leq
  deriving (Show, Eq)
