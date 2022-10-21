{-# LANGUAGE LambdaCase #-}

module DumbCC.Parser.Expr where

import qualified DumbCC.Lexer as L
import DumbCC.Parser.ParenTree hiding (Parser)
import DumbCC.Parser.Types.Sugared
import DumbCC.Parser.Utils hiding (Parser)
import qualified DumbCC.Parser.Utils as U

type Parser = U.Parser ParserState

data ParserState = ParserState
  { valStack :: [ExprS],
    opStack :: [BOp],
    unaryStack :: [UOp]
  }
  deriving (Show, Eq)

mapValStack :: ([ExprS] -> (a, [ExprS])) -> Parser a
mapValStack f =
  state'
    ( \x ->
        let (a, s') = f (valStack x)
         in (a, x {valStack = s'})
    )

mapOpStack :: ([BOp] -> (a, [BOp])) -> Parser a
mapOpStack f =
  state'
    ( \x ->
        let (a, s') = f (opStack x)
         in (a, x {opStack = s'})
    )

popVal :: Parser (Maybe ExprS)
popVal =
  mapValStack
    ( \case
        (x : xs) -> (Just x, xs)
        [] -> (Nothing, [])
    )

parseExpr :: [PTree] -> Either String ExprS
parseExpr = undefined

readToken :: PTree -> Parser ()
readToken (PTParen pType expr) = do
  subExpr <- liftEither' $ parseExpr expr
  pushVal (Just pType) subExpr
readToken (PTTok tok) = case tok of
  L.TNum x -> pushVal Nothing $ ELit (LNum x)
  L.TStr s -> pushVal Nothing $ ELit (LStr s)
  L.TId n -> pushVal Nothing $ EId n
  L.TPnc pnc ->
    get' >>= \case
      ParserState [] [] us -> do
        uop <- undefined
        put' $ ParserState [] [] (uop : us)
      ParserState (v : vs) (b : bs) us -> do
        op <- undefined
        if b < op
          then put' $ ParserState (v : vs) (op : b : bs) us
          else reduceBOps
      _ -> undefined

pushVal :: (Maybe Paren) -> ExprS -> Parser ()
pushVal pType x =
  get' >>= \case
    -- first item in expression
    ParserState [] [] us -> do
      if pType `elem` [Nothing, Just Paren]
        then put' $ ParserState [applyUOps us x] [] []
        else fail $ "Unexpected parentheses of type " ++ show pType
    -- expression before, has operation after
    ParserState (f : xs) [] [] -> case pType of
      Just Paren -> put' $ ParserState (EApp f x : xs) [] []
      Just Brac -> put' $ ParserState (EIdx f x : xs) [] []
      Just Curl -> fail "Unexpected curly brace in expression"
      Nothing -> fail $ "Unexpected unbraced expression" ++ show x
    ParserState (a : xs) (b : bs) us ->
      put' $
        ParserState
          (EBi b a (applyUOps us x) : xs)
          bs
          []
    st -> fail $ "Could not push val " ++ show x ++ " for state " ++ show st

reduceBOps :: Parser ()
reduceBOps = undefined

applyUOps :: [UOp] -> ExprS -> ExprS
applyUOps [] x = x
applyUOps (u : us) x = applyUOps us (EUn u x)