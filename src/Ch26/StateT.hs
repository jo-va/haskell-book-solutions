{-# LANGUAGE InstanceSigs #-}

module Ch26.StateT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT' s m a =
    StateT' { runStateT' :: s -> m (a, s) }

instance Functor m => Functor (StateT' s m) where
    fmap f (StateT' smas) = StateT' $ \s ->
        fmap (\(a, s') -> (f a, s')) $ smas s

instance Monad m => Applicative (StateT' s m) where
    pure a = StateT' $ \s -> pure (a, s)

    (<*>) :: StateT' s m (a -> b)
          -> StateT' s m a
          -> StateT' s m b
    StateT' smf <*> StateT' sma = StateT' $ \s -> do
        (f, s') <- smf s
        fmap (\(a, s'') -> (f a, s'')) $ sma s'

instance Monad m => Monad (StateT' s m) where
    return = pure

    (>>=) :: StateT' s m a
          -> (a -> StateT' s m b)
          -> StateT' s m b
    StateT' smf >>= f = StateT' $ \s -> do
        (a, s') <- smf s
        runStateT' (f a) s'

instance MonadTrans (StateT' s) where
    lift ma = StateT' $ \s -> do
        a <- ma
        return (a, s)

instance MonadIO m => MonadIO (StateT' s m) where
    liftIO = lift . liftIO
