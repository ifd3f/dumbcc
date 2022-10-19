{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DumbCC.Parser.Expr where

import Control.Monad.Except
import Control.Monad.State
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.Types.Sugared
import qualified DumbCC.Parser.Types.Sugared as P

parseExpr :: [Token] -> (Maybe ExprS)
parseExpr = undefined

data ParserState = ParserState
  { exprs :: [(Bool, ExprS)],
    ops :: [BOp],
    wasBinary :: Bool
  }

newtype Parser a = Parser
  { runParser :: ExceptT String (State ParserState) a
  }
  deriving (Functor, Applicative, Monad)

liftGet :: (ParserState -> a) -> Parser a
liftGet f = Parser $ lift (f <$> get)

state' :: (ParserState -> (a, ParserState)) -> Parser a
state' f = Parser $ lift (state f)

getExprs :: Parser [(Bool, ExprS)]
getExprs = liftGet exprs

getOps :: Parser [BOp]
getOps = liftGet ops

getWasBinary :: Parser Bool
getWasBinary = liftGet wasBinary

shift :: Token -> Parser (Either String ())
shift t = do
  ge <- getExprs
  o <- getOps
  pure $ case (ge, o, t) of
    ([], TId n) -> Right ([Ex (EId n)], ts)
    ([], TNum l) -> Right ([Ex (ELit (LNum l))], ts)
    ([], TStr s) -> Right ([Ex (ELit (LStr s))], ts)
    ([], TPnc L.Add) -> Right ([SUn P.Add], ts)
    ([], TPnc L.Sub) -> Right ([SUn P.Sub], ts)
    ([], TPnc L.Mul) -> Right ([SUn P.Deref], ts)
    ([], TPnc L.BAnd) -> Right ([SUn P.Ref], ts)
    (ss', TPnc L.LParen) -> Right (OpenParen : ss', ts)
    _ -> Left "Invalid start symbol"

-- reduce ss = case ss of
--   (Ex _ x : SUn op : ss') -> (Ex False (EUn op x)) : ss'
--   (Ex _ b : SBi op : Ex _ a : ss') -> (Ex False (EBi op a b)) : ss'
--   (Ex True x : Ex False f : ss') -> (Ex False (EApp f x)) : ss'
--   _ -> undefined