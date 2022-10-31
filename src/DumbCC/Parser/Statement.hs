{-# LANGUAGE LambdaCase #-}

module DumbCC.Parser.Statement where

import DumbCC.Lexer
import DumbCC.Parser.ParenTree hiding (Parser)
import DumbCC.Parser.Types.Sugared
import DumbCC.Parser.Utils hiding (Parser)
import qualified DumbCC.Parser.Utils as U

type Parser = U.Parser [PTree]

takeProgram :: Parser SProg
takeProgram = ta

takeFunc :: Parser SFunc
takeFunc = undefined

takeStmt :: Parser SStmt
takeStmt = undefined

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

takeExprStmt :: Parser SStmt
takeExprStmt = do
  expr <- takeExpr
  _ <-
    get' >>= \case
      PTTok (TPnc Semicolon) : rest -> put' rest
      _ -> fail "Expected semicolon"
  pure $ SSingleExpr expr

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
