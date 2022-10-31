{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DumbCC.Parser.Statement where

import Control.Lens
import Control.Lens.TH
import Data.Set (Set)
import qualified Data.Set as S
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.Expr (parseExpr)
import DumbCC.Parser.ParenTree hiding (Parser)
import DumbCC.Parser.Types.Sugared
import DumbCC.Parser.Utils hiding (Parser)
import qualified DumbCC.Parser.Utils as U

type Parser = U.Parser PS

data PS = PS
  { _psTokens :: [PTree],
    -- | set of types
    _psTypes :: Set String
  }

makeLenses ''PS

takeProgram :: Parser SProg
takeProgram = undefined

takeFunc :: Parser SFunc
takeFunc = undefined

takeStmt :: Parser SStmt
takeStmt = undefined

takeSemicolonStatement :: Parser SStmt
takeSemicolonStatement =
  get' >>= \st -> case st ^. psTokens of
    [] -> fail "Unexpected EOF"
    ts -> case span (/= (PTTok (TPnc L.Semicolon))) ts of
      (stmt, x : xs) -> do
        put' (ps & psTokens .~ (x : xs))
        parseSemicolonStatement stmt
      _ -> fail "Expected semicolon"

parseSemicolonStatement :: [PTree] -> Parser SStmt
parseSemicolonStatement [] = pure SEmpty
parseSemicolonStatement ts = case ts of
  ((PTTok (TId tname)) : rest) ->
    get' >>= \st ->
      if tname `S.member` (st ^. psTypes)
        then parseDecl tname rest
        else parseExprStmt
  _ -> parseExprStmt
  where
    parseExprStmt = undefined -- pure $ parseExpr ts

parseDecl :: String -> [PTree] -> Parser SStmt
parseDecl
  tname
  ( (PTTok (TId vname))
      : (PTTok (TPnc Asgn))
      : _
    ) =
    SDecl tname vname <$> expr'
    where
      expr' = undefined -- expr
parseDecl _ _ = fail $ "Could not parse type declaration"

takeBlock :: Parser [SStmt]
takeBlock =
  takeNext >>= \case
    PTParen Curl children ->
      liftEither' $ evalParser children takeStmts
    _ -> undefined

takeStmts :: Parser [SStmt]
takeStmts = reverse <$> stmts
  where
    stmts =
      get' >>= \case
        [] -> pure []
        _ -> do
          s <- takeStmt
          rest <- takeStmts
          pure (s : rest)

takeIfStmt :: Parser SStmt
takeIfStmt = do
  ifCond <-
    get' >>= \case
      (PTTok (TId "if")) : (PTParen Paren cond) : rest -> do
        put' rest
        parseExpr <$> cond
      _ -> fail "not an if statement"
  body <- takeStmt
  elseSection <-
    get' >>= \case
      (PTTok (TId "else")) : rest -> do
        put' rest
        Just <$> takeStmt
      _ -> pure Nothing
  pure $ SIf ifCond body elseSection

takeNext :: Parser PTree
takeNext = do
  (t : ts') <- get'
  put' ts'
  pure t

peekNext :: Parser PTree
peekNext = do
  (t : ts') <- get'
  put' ts'
  pure t
