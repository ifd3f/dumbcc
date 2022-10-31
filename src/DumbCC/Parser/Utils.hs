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

get' :: Parser s s
get' = Parser $ lift get

gets' :: (s -> a) -> Parser s a
gets' f = Parser $ lift (f <$> get)

state' :: (s -> (a, s)) -> Parser s a
state' f = Parser $ lift (state f)

mapState :: (s -> s) -> Parser s ()
mapState f = Parser $ lift (modify f)

put' :: s -> Parser s ()
put' s = Parser $ lift (put s)

liftEither' :: Either String a -> Parser s a
liftEither' = Parser . liftEither

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