{-# LANGUAGE QuasiQuotes #-}

module DumbCC.Lexer where

import Control.Monad.State
import Data.Char
import Text.RawString.QQ
import Text.Regex.TDFA

data Token
  = TId String
  | TNum String
  | TStr String
  | TPnc Punct
  deriving (Show, Eq)

data Punct
  = Comma
  | Semicolon
  | Question
  | Colon
  | LCurl
  | RCurl
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Add
  | Sub
  | Mul
  | Div
  | BAnd
  | BOr
  | BNot
  | LAnd
  | LOr
  | LNot
  | Xor
  | Inc
  | Dec
  | Asgn
  | Eq
  | Neq
  | Ge
  | Le
  | Geq
  | Leq
  deriving (Show, Eq)

lex :: String -> [(Int, Token)]
lex s = evalState takeLex (0, s)

takeLex :: Tokenizer [(Int, Token)]
takeLex = do
  _ <- takeSpace
  i <- peekPos
  t <- takeToken
  case t of
    Just t' -> do
      rest <- takeLex
      pure $ (i, t') : rest
    Nothing -> pure []

type Tokenizer = State (Int, String)

peekChar :: Tokenizer (Maybe Char)
peekChar = do
  (_, s) <- get
  pure $ case s of
    "" -> Nothing
    x : _ -> Just x

peekPos :: Tokenizer Int
peekPos = gets (\(i, _) -> i)

takeChar :: Tokenizer (Maybe Char)
takeChar = do
  (i, s) <- get
  case s of
    "" -> pure Nothing
    x : xs -> do
      put (i + 1, xs)
      pure $ Just x

takeRegex :: String -> Tokenizer String
takeRegex regex = do
  (i, s) <- get
  let (pre, result, post) = s =~ (regex) :: (String, String, String)
  case pre of
    "" -> do
      put (i + length result, post)
      pure result
    _ -> pure ""

takeToken :: Tokenizer (Maybe Token)
takeToken =
  (TPnc <$$> takePunct)
    <||> (TNum <$$> takeFloat)
    <||> (TPnc <$$> takeOp)
    <||> (TId <$$> takeId)

-- | Double fmap
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
a <$$> b = (fmap a) <$> b

-- | Short-circuiting orElse with monads.
(<||>) :: Monad m => m (Maybe b) -> m (Maybe b) -> m (Maybe b)
a <||> b = do
  a' <- a
  case a' of
    Nothing -> b
    Just x -> pure $ Just x

takeId :: Tokenizer (Maybe String)
takeId = do
  s <- takeRegex "^[a-zA-Z][a-zA-Z0-9_]*"
  pure $ case s of
    "" -> Nothing
    s' -> Just s'

takeFloat :: Tokenizer (Maybe String)
takeFloat = do
  s <- takeRegex "^[+-]?([0-9]*[.])?[0-9]+f?"
  pure $ case s of
    "" -> Nothing
    s' -> Just s'

takeSpace :: Tokenizer ()
takeSpace = do
  pc <- peekChar
  case pc of
    Just c ->
      if isSpace c
        then do
          _ <- takeChar
          takeSpace
        else pure ()
    _ -> pure ()

takePunct :: Tokenizer (Maybe Punct)
takePunct = do
  -- Peek the character on top
  c <- peekChar
  let result = case c of
        Just ',' -> Just Comma
        Just ';' -> Just Semicolon
        Just '?' -> Just Question
        Just ':' -> Just Colon
        Just '{' -> Just LCurl
        Just '}' -> Just RCurl
        Just '[' -> Just LBrace
        Just ']' -> Just RBrace
        Just '(' -> Just LParen
        Just ')' -> Just RParen
        _ -> Nothing

  -- If it was a punctuation character, then move forward.
  case result of
    Just _ -> do _ <- takeChar; pure ()
    _ -> pure ()
  pure result

takeOp :: Tokenizer (Maybe Punct)
takeOp = do
  s <- takeRegex [r|^[\+-\*/=&|^><]{1,2}|]
  pure $ case s of
    "+" -> Just Add
    "-" -> Just Sub
    "*" -> Just Mul
    "/" -> Just Div
    "&" -> Just BAnd
    "|" -> Just BOr
    "~" -> Just BNot
    "&&" -> Just LAnd
    "||" -> Just LOr
    "!" -> Just LNot
    "^" -> Just Xor
    "++" -> Just Inc
    "--" -> Just Dec
    "=" -> Just Asgn
    "==" -> Just Eq
    "!=" -> Just Neq
    ">" -> Just Ge
    "<" -> Just Le
    ">=" -> Just Geq
    "<=" -> Just Leq
    _ -> Nothing