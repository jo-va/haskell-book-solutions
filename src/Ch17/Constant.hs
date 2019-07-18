module Ch17.Constant where

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant { getConstant = mempty }
    (<*>) (Constant x) (Constant y) = Constant (mappend x y)
