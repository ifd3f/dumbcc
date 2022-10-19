{-# LANGUAGE LambdaCase #-}

module DumbCC.Parser.Sugared where

import DumbCC.Parser.ParenTree hiding (Parser)
import DumbCC.Parser.Types.Sugared (SFunc, SProg, SStmt)
import DumbCC.Parser.Utils (evalParser, get', liftEither', put')
import qualified DumbCC.Parser.Utils as U

type Parser = U.Parser [PTree]

readProgram :: Parser SProg
readProgram = undefined

readFunc :: Parser SFunc
readFunc = undefined

readStmt :: Parser SStmt
readStmt = undefined

readBlock :: Parser [SStmt]
readBlock =
  readNext >>= \case
    PTParen Curl children ->
      liftEither' $ evalParser children readStmts
    _ -> undefined

readStmts :: Parser [SStmt]
readStmts = reverse <$> stmts
  where
    stmts =
      get' >>= \case
        [] -> pure []
        _ -> do
          s <- readStmt
          rest <- readStmts
          pure (s : rest)

readNext :: Parser PTree
readNext = do
  (t : ts') <- get'
  put' ts'
  pure t