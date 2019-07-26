{-# LANGUAGE InstanceSigs #-}

module Ch22.Person where

import Control.Applicative (liftA2)

newtype Reader r a =
    Reader { runReader' :: r -> a }

newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person =
    Person {
      humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
      dogsName    :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Reader Person Dog
getDogR' = Reader $ Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
    d <- dogName
    a <- address
    return $ Dog d a

getDogRM' :: Reader Person Dog
getDogRM' = Reader $ getDogRM

getDogRL :: Person -> Dog
getDogRL = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader $ const x

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    Reader rab <*> Reader ra =
        Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> Reader r b
    Reader ra >>= aRb =
        Reader $ \r -> runReader' (aRb $ ra r) r
