module DumbCC.Parser.Types.Sugared where

data ExprS
  = ELit Lit
  | EId String
  | EUn UOp ExprS
  | EBi BOp ExprS ExprS
  | EApp ExprS ExprS
  | EIdx ExprS ExprS
  deriving (Show, Eq)

data Prog s
  = Prog [Func s]
  deriving (Show, Eq)

data Func s = Func
  { fName :: String,
    fRet :: String,
    fArgs :: [(String, String)],
    fBody :: s
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- | A desugared statement
data CStmt
  = CScope [CStmt]
  | CBreak
  | CSingleExpr ExprS
  | CAsgn String ExprS
  | CDecl String String ExprS
  | CIf ExprS SStmt SStmt
  | CWhile ExprS SStmt
  deriving (Show, Eq)

data Lit = LStr String | LNum String
  deriving (Show, Eq)

data UOp
  = Inc
  | Dec
  | Neg
  | Pos
  | Deref
  | Ref
  deriving (Show, Eq)

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
