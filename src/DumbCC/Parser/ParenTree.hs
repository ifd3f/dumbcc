module DumbCC.Parser.ParenTree where

import DumbCC.Lexer

-- | Parentheses tree
data PTree
  = PTParen Paren [PTree]
  | PTTok Token
  deriving (Show, Eq)

data StackState
  = StOpen Paren
  | StPT PTree
  deriving (Show, Eq)

data Paren = Curl | Brac | Paren
  deriving (Show, Eq)

detectParens :: [Token] -> Either String [PTree]
detectParens [] = Right []
detectParens toks = reverse <$> detectParens' [] toks
  where
    detectParens' stack [] = reduceEmpty stack
    detectParens' stack (t : ts) = do
      stack' <- readToken stack t
      detectParens' stack' ts

readToken :: [StackState] -> Token -> Either String [StackState]
readToken stack tok = case tok of
  (TPnc LBrac) -> pure $ StOpen Brac : stack
  (TPnc LCurl) -> pure $ StOpen Curl : stack
  (TPnc LParen) -> pure $ StOpen Paren : stack
  (TPnc RBrac) -> reduce Brac
  (TPnc RCurl) -> reduce Curl
  (TPnc RParen) -> reduce Paren
  other -> pure $ StPT (PTTok other) : stack
  where
    reduce paren = do
      (children, stack') <- reduceParen paren stack
      pure $ (StPT (PTParen paren children)) : stack'

-- | Returns (the paren's children, rest of stack)
reduceParen :: Paren -> [StackState] -> Either String ([PTree], [StackState])
reduceParen target (StOpen paren : xs)
  -- We found the correct opening paren
  | paren == target = Right ([], xs)
  -- We found a different opening paren
  | otherwise =
      Left $ show paren ++ " is closed by " ++ show target
-- Just a normal token
reduceParen target (StPT pt : xs) = do
  (children, xs') <- reduceParen target xs
  pure (pt : children, xs')
-- Unexpected EOF
reduceParen target [] = Left $ "Dangling closing " ++ show target

reduceEmpty :: [StackState] -> Either String [PTree]
reduceEmpty [] = Right []
reduceEmpty (StOpen paren : _) = Left $ "Unclosed open " ++ show paren
reduceEmpty (StPT pt : xs) = do
  rest <- reduceEmpty xs
  pure (pt : rest)