module DumbCC.Lexer where

import Control.Monad.State
import Data.Char

data Token
  = TKeyword String
  | TId String
  | TConst String
  | TStr String
  | TOp String
  | TPunct String

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

readChar :: Tokenizer (Maybe Char)
readChar = do
  (i, s) <- get
  case s of
    "" -> pure Nothing
    x : xs -> do
      put (i + 1, xs)
      pure $ Just x

readId :: Tokenizer String
readId = do
  res <- readChar
  case res of
    Just c ->
      if isLetter c
        then do
          rest <- readId
          pure $ c : rest
        else pure ""
    Nothing -> pure ""

readInt :: Tokenizer (Maybe String)
readInt = do
  res <- readChar
  case res of
    Just c ->
      if isLetter c
        then do
          rest <- readId
          pure $ Just (c : rest)
        else pure (Just "")
    Nothing -> pure Nothing
