{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DumbCC.Parser.Utils where

import Control.Monad.Except
import Control.Monad.State

-- | A generalized parser type with an internal state that may error.
newtype Parser s a = Parser
  { runParser :: ExceptT String (State s) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadFail (Parser s) where
  fail err = Parser $ throwError err

liftGet :: (s -> a) -> Parser s a
liftGet f = Parser $ lift (f <$> get)

state' :: (s -> (a, s)) -> Parser s a
state' f = Parser $ lift (state f)