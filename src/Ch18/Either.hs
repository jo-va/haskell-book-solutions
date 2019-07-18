module Ch18.Either where

import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (Either' a) where
    fmap = liftM

instance Applicative (Either' a) where
    pure = return 
    (<*>) = ap

instance Monad (Either' a) where
    return = Right'
    Left' a  >>= _ = Left' a
    Right' a >>= f = f a

trigger :: Either String (Int, Int, String)
trigger = undefined

spec :: IO ()
spec = quickBatch $ monad trigger
