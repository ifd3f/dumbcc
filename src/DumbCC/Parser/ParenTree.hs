{-# LANGUAGE LambdaCase #-}

module DumbCC.Parser.ParenTree where

import Control.Monad (forM)
import DumbCC.Lexer
import DumbCC.Parser.Utils hiding (Parser)
import qualified DumbCC.Parser.Utils as U

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

type Parser = U.Parser [StackState]

push :: StackState -> Parser ()
push t = do
  stack <- get'
  put' (t : stack)

pop :: Parser (Maybe StackState)
pop = do
  stack <- get'
  case stack of
    (t : stack') -> do
      put' stack'
      pure $ Just t
    [] -> pure Nothing

detectParens :: [Token] -> Either String [PTree]
detectParens toks = evalParser [] $ do
  _ <- forM toks readToken
  items <- reduceEmpty
  pure $ reverse items

-- | Reads a single token into the parser.
readToken :: Token -> Parser ()
readToken tok = case tok of
  (TPnc LBrac) -> push $ StOpen Brac
  (TPnc LCurl) -> push $ StOpen Curl
  (TPnc LParen) -> push $ StOpen Paren
  (TPnc RBrac) -> reduce Brac
  (TPnc RCurl) -> reduce Curl
  (TPnc RParen) -> reduce Paren
  other -> push $ StPT (PTTok other)

reduce :: Paren -> Parser ()
reduce paren = do
  result <- reduce' paren
  push $ StPT $ PTParen paren (reverse result)

-- | Returns the paren's children IN REVERSE.
reduce' :: Paren -> Parser [PTree]
reduce' target =
  pop >>= \case
    -- Unexpected EOF
    Nothing -> fail $ "Dangling closing " ++ show target
    -- We found an opening paren
    Just (StOpen paren) ->
      if paren == target -- Is it the type we're looking for?
        then pure []
        else fail $ show paren ++ " is incorrectly closed by " ++ show target
    -- Just a normal token
    Just (StPT pt) -> do
      children <- reduce' target
      pure (pt : children)

-- | Reduces the stack, assuming it has seen EOF
-- | Returns the top-level values, IN REVERSE.
reduceEmpty :: Parser [PTree]
reduceEmpty =
  pop >>= \case
    Nothing -> pure []
    Just (StOpen paren) -> fail $ "Unclosed open " ++ show paren
    Just (StPT pt) -> do
      rest <- reduceEmpty
      pure (pt : rest)