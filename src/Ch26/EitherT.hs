module Ch26.EitherT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) =
        EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure

    EitherT f <*> EitherT ma =
        EitherT $ (<*>) <$> f <*> ma

instance Monad m => Monad (EitherT e m) where
    return = pure

    EitherT ma >>= f = EitherT $ do
        v <- ma
        case v of
          Left e -> return $ Left e
          Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT amb) =
    amb >>= either amc bmc

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap return

instance MonadIO m => MonadIO (EitherT e m) where
    liftIO = lift . liftIO
