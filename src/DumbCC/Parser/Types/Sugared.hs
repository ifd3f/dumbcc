{-# LANGUAGE DeriveFunctor #-}

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
  deriving (Show, Eq, Functor)

data Func s = Func
  { fName :: String,
    fRet :: String,
    fArgs :: [(String, String)],
    fBody :: s
  }
  deriving (Show, Eq, Functor)

type SProg = Prog SStmt

type SFunc = Func SStmt

-- | A sugared statement
data SStmt
  = SScope [SStmt]
  | SBreak
  | SSingleExpr ExprS
  | SAsgn String ExprS
  | SDecl String String ExprS
  | SIf ExprS SStmt (Maybe SStmt)
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
  | BNot
  | LNot
  deriving (Show, Eq)

data BOp
  = Add
  | Sub
  | Mul
  | Div
  | BAnd
  | BOr
  | LAnd
  | LOr
  | Xor
  | Eq
  | Neq
  | Ge
  | Le
  | Geq
  | Leq
  | Dot
  | Arrow
  | Modulo
  deriving (Show, Eq)

orderOfOperations =
  [ [Arrow, Dot],
    [Mul, Div, Modulo],
    [Add, Sub],
    [BAnd, BOr, Xor],
    [LAnd, LOr]
  ]

instance Ord BOp where
  Add `compare` Sub = EQ
  Mul `compare` Div = EQ
  BAnd `compare` BOr = EQ
  BAnd `compare` BOr = EQ
  a `compare` b = b `compare` a
