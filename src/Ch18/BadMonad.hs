module Ch18.BadMonad where

import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap = liftM

instance Applicative CountMe where
    pure = return
    (<*>) = ap

instance Monad CountMe where
    return = CountMe 0
    CountMe n a >>= f =
        let CountMe n' b = f a
         in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

spec :: IO ()
spec = do
    let trigger = undefined :: CountMe (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
