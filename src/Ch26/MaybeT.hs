{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch26.MaybeT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma

instance forall m .
    Applicative m => Applicative (MaybeT m) where
    pure :: forall a . a -> MaybeT m a
    pure = MaybeT . pure . pure

    (<*>) :: forall a b .
             MaybeT m (a -> b)
          -> MaybeT m a
          -> MaybeT m b
    MaybeT f <*> MaybeT ma =
        let liftApply :: m (Maybe (a -> b))
                      -> m (Maybe a -> Maybe b)
            liftApply func = (<*>) <$> func

            apF :: m (Maybe a -> Maybe b)
            apF = liftApply f

            apApF :: m (Maybe a) -> m (Maybe b)
            apApF = (<*>) apF

         in MaybeT $ apApF ma
         -- MaybeT $ (<*>) <$> f <*> ma

instance Monad m => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b
    MaybeT ma >>= f = MaybeT $ do
        v <- ma
        case v of
          Just a -> runMaybeT (f a)
          Nothing -> return Nothing

instance MonadTrans MaybeT where
    lift = MaybeT . fmap return

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
