{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DumbCC.Parser.Expr where

import DumbCC.Lexer
import DumbCC.Parser.Types.Sugared
import qualified DumbCC.Parser.Utils as U
import DumbCC.Parser.Utils hiding (Parser)

parseExpr :: [Token] -> (Maybe ExprS)
parseExpr = undefined

data PS = PS
  { exprStack :: [(Bool, ExprS)],
    opStack :: [BOp],
    wasBinary :: Bool
  }

data StackState = 
  StEx Bool ExprS
  | StUn UOp
  | StBi BOp
  | StOpenParen

type Parser = U.Parser PS

getExprStack :: Parser [(Bool, ExprS)]
getExprStack = liftGet exprStack

getOpStack :: Parser [BOp]
getOpStack = liftGet opStack

getWasBinary :: Parser Bool
getWasBinary = liftGet wasBinary

setState :: PS -> Parser ()
setState s = state' (\_ -> ((), s))

shift :: Token -> Parser ()
shift t = do
  es <- getExprStack
  os <- getOpStack
  case t of
    TId n -> setState (PS ((False, EId n) : es) os False)
    TStr s -> setState (PS ((False, ELit (LStr s)) : es) os False)
    TNum x -> setState (PS ((False, ELit (LNum x)) : es) os False)
    TPnc _ -> undefined -- TODO

--    ([], TId n) -> Right ([Ex (EId n)], ts)
--    ([], TNum l) -> Right ([Ex (ELit (LNum l))], ts)
--    ([], TStr s) -> Right ([Ex (ELit (LStr s))], ts)
--    ([], TPnc L.Add) -> Right ([SUn P.Add], ts)
--    ([], TPnc L.Sub) -> Right ([SUn P.Sub], ts)
--    ([], TPnc L.Mul) -> Right ([SUn P.Deref], ts)
--    ([], TPnc L.BAnd) -> Right ([SUn P.Ref], ts)
--    (ss', TPnc L.LParen) -> Right (OpenParen : ss', ts)
--    _ -> Left "Invalid start symbol"

-- reduce ss = case ss of
--   (Ex _ x : SUn op : ss') -> (Ex False (EUn op x)) : ss'
--   (Ex _ b : SBi op : Ex _ a : ss') -> (Ex False (EBi op a b)) : ss'
--   (Ex True x : Ex False f : ss') -> (Ex False (EApp f x)) : ss'
--   _ -> undefined