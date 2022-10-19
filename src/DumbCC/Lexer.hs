{-# LANGUAGE QuasiQuotes #-}

module DumbCC.Lexer where

import Control.Monad.State
import Text.RawString.QQ
import Text.Regex.TDFA

data Token
  = TId String
  | TFloat Double
  | TInt Integer
  | TStr String
  | TOp Op
  | TPunct Punct

data Punct
  = Comma
  | Semicolon
  | Colon
  | LCurl
  | RCurl
  | LBrace
  | RBrace
  | LParen
  | RParen

data Op
  = Add
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

lex :: String -> [Token]
lex "" = []

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
  let (_, result, rest) = s =~ (regex) :: (String, String, String)
  put (i, rest)
  pure result

takeToken :: Tokenizer (Maybe Token)
takeToken =
  (TPunct <$$> takePunct)
    <||> (TOp <$$> takeOp)
    <||> (TFloat <$$> takeFloat)
    <||> (TId <$$> takeId)
    <||> (TInt <$$> takeInt)

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

takeInt :: Tokenizer (Maybe Integer)
takeInt = do
  s <- takeRegex [r|^\d+|]
  pure $ case s of
    "" -> Nothing
    s' -> Just (read s' :: Integer)

takeFloat :: Tokenizer (Maybe Double)
takeFloat = do
  s <- takeRegex "^[+-]?([0-9]*[.])?[0-9]+f?"
  pure $ case s of
    "" -> Nothing
    s' -> Just (read s' :: Double)

takeSpace :: Tokenizer String
takeSpace = takeRegex [r|^\w+|]

takePunct :: Tokenizer (Maybe Punct)
takePunct = do
  -- Peek the character on top
  c <- peekChar
  let result = case c of
        Just ',' -> Just Comma
        Just ';' -> Just Semicolon
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

takeOp :: Tokenizer (Maybe Op)
takeOp = do
  s <- takeRegex [r|^[+-*/=&|^><]+|]
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