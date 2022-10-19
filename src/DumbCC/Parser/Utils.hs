{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DumbCC.Parser.Utils where

import Control.Monad.Except
import Control.Monad.State

-- | A generalized parser type with an internal state that may error.
newtype Parser s a = Parser
  { _runParser :: ExceptT String (State s) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadFail (Parser s) where
  fail err = Parser $ throwError err

runParser :: s -> Parser s a -> (Either String a, s)
runParser = flip $ runState . runExceptT . _runParser

evalParser :: s -> Parser s a -> Either String a
evalParser = flip $ evalState . runExceptT . _runParser

execParser :: s -> Parser s a -> s
execParser = flip $ execState . runExceptT . _runParser

get' :: Parser a a
get' = Parser $ lift get

gets' :: (s -> a) -> Parser s a
gets' f = Parser $ lift (f <$> get)

state' :: (s -> (a, s)) -> Parser s a
state' f = Parser $ lift (state f)

put' :: s -> Parser s ()
put' s = Parser $ lift (put s)