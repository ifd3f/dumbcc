{-# LANGUAGE QuasiQuotes #-}

module DumbCC.Lexer where

import Control.Monad.State
import Data.Char
import Data.List (isPrefixOf)
import Text.Regex.TDFA
import DumbCC.Parser.Utils

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
  | LBrac
  | RBrac
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

-- | Checks if the input stream has the given prefix.
-- | If it does, returns True and advances the read head past the prefix.
-- | Otherwise, returns False and does nothing.
takePrefix :: String -> Tokenizer Bool
takePrefix str = do
  (i, s) <- get
  if str `isPrefixOf` s
    then do
      put (i + length str, drop (length str) s)
      pure True
    else pure False

takeToken :: Tokenizer (Maybe Token)
takeToken =
  (TPnc <$$> takePunct)
    <||> (TNum <$$> takeFloat)
    <||> (TId <$$> takeId)

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
        then do _ <- takeChar; takeSpace
        else pure ()
    _ -> pure ()

takePunct :: Tokenizer (Maybe Punct)
takePunct = tryList syms
  where
    tryList :: [(String, Punct)] -> Tokenizer (Maybe Punct)
    tryList [] = pure Nothing
    tryList ((sym, out) : rest) = do
      result <- takePrefix sym
      if result
        then pure $ Just out
        else tryList rest
    syms =
      [ ("&&", LAnd),
        ("||", LOr),
        ("++", Inc),
        ("--", Dec),
        ("==", Eq),
        ("!=", Neq),
        (">=", Geq),
        ("<=", Leq),
        ("+", Add),
        ("-", Sub),
        ("*", Mul),
        ("/", Div),
        ("&", BAnd),
        ("|", BOr),
        ("~", BNot),
        ("!", LNot),
        ("^", Xor),
        ("=", Asgn),
        (">", Ge),
        ("<", Le),
        (",", Comma),
        (";", Semicolon),
        ("?", Question),
        (":", Colon),
        ("{", LCurl),
        ("}", RCurl),
        ("[", LBrac),
        ("]", RBrac),
        ("(", LParen),
        (")", RParen)
      ]