{-# LANGUAGE LambdaCase #-}

module DumbCC.Parser.Expr where

import Data.List.Split
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.ParenTree hiding (Parser, reduce)
import DumbCC.Parser.Types.Sugared
import qualified DumbCC.Parser.Types.Sugared as T
import DumbCC.Parser.Utils hiding (Parser)
import qualified DumbCC.Parser.Utils as U

type Parser = U.Parser PS

data PS = PS
  { tokens :: [PTree],
    valStack :: [ExprS],
    opStack :: [BOp],
    unaryStack :: [UOp]
  }
  deriving (Show, Eq)

parseExpr :: [PTree] -> Either String ExprS
parseExpr = undefined

parseArgs :: [PTree] -> Either String [ExprS]
parseArgs [] = pure []
parseArgs ts = traverse parseExpr args
  where
    args = splitOn [PTTok (TPnc L.Comma)] ts

shift :: Parser ()
shift =
  popTok >>= \case
    -- We have reached end of the stream
    Nothing -> pure ()
    -- Found a parenthesized expression
    Just (PTParen Paren expr) -> do
      subExpr <- liftEither' $ parseExpr expr
      pushValAndReduce subExpr
    -- Wrong type of parentheses
    Just (PTParen otherPType expr) ->
      fail $ "Unexpected parens " ++ show otherPType ++ " containing " ++ show expr
    -- Found a token
    Just (PTTok tok) -> case tok of
      L.TNum x -> pushValAndReduce $ ELit (LNum x)
      L.TStr s -> pushValAndReduce $ ELit (LStr s)
      L.TId n -> pushValAndReduce $ EId n
      L.TPnc pnc ->
        get' >>= \case
          PS ts [] [] us -> do
            uop <- case pnc of
              L.Add -> pure T.Pos
              L.Sub -> pure T.Neg
              other -> fail $ "Unknown unary operator " ++ show other
            put' $ PS ts [] [] (uop : us)
          PS ts (v : vs) (b : bs) us -> do
            op <- case pnc of
              L.Add -> pure T.Add
              L.Sub -> pure T.Sub
              other -> fail $ "Unknown binary operator " ++ show other
            if b < op
              then put' $ PS ts (v : vs) (op : b : bs) us
              else reduceBOps
          _ -> undefined

pushValAndReduce :: ExprS -> Parser ()
pushValAndReduce x =
  -- Lookahead and see if there's a function call, index, or postinc/postdec
  popTok >>= \case
    -- There are parens immediately afterwards
    Just (PTParen pType args) ->
      case pType of
        Curl -> fail "Unexpected curly brace in expression"
        -- Function call
        Paren -> do
          args' <- liftEither' $ parseArgs args
          pushValAndReduce (EApp x args')
        -- Index
        Brac -> do
          args' <- liftEither' $ parseExpr args
          pushValAndReduce (EIdx x args')
    -- Postinc
    Just (PTTok (TPnc L.Inc)) -> pushValAndReduce (EUn PostInc x)
    -- Postdec
    Just (PTTok (TPnc L.Dec)) -> pushValAndReduce (EUn PostDec x)
    -- Unknown other token, push it back on
    Just other -> do pushTok other; reduce
    Nothing -> reduce
  where
    reduce =
      get' >>= \case
        -- first item in expression
        PS toks [] [] us ->
          put' $ PS toks [applyUOps us x] [] []
        -- expression before without binary ops
        PS _ _ [] _ ->
          fail $ "Unexpected unbraced expression" ++ show x
        -- expression before with binary ops in stack
        PS toks (a : xs) (b : bs) us ->
          put' $
            PS
              toks
              (EBi b a (applyUOps us x) : xs)
              bs
              []
        st -> fail $ "Could not push val " ++ show x ++ " for state " ++ show st

popTok :: Parser (Maybe PTree)
popTok =
  get' >>= \case
    PS (t : ts) x b u -> do
      put' (PS ts x b u)
      pure (Just t)
    _ -> pure Nothing

pushTok :: PTree -> Parser ()
pushTok t =
  get' >>= \(PS ts x b u) -> do
    put' (PS (t : ts) x b u)

reduceBOps :: Parser ()
reduceBOps =
  get' >>= \(PS tok xs os us) ->
    case (xs, os) of
      (a : b : xs', o : os') -> do
        put' $ PS tok xs' os' []
        pushValAndReduce (EBi o a (applyUOps us b))
      other -> fail $ "Could not push val for state " ++ show other

applyUOps :: [UOp] -> ExprS -> ExprS
applyUOps [] x = x
applyUOps (u : us) x = applyUOps us (EUn u x)
