{-# LANGUAGE InstanceSigs #-}

module Result(Result(..)) where

data Result a b = Err a | Ok b deriving (Eq, Show, Read)

instance Functor (Result e) where
    fmap :: (a -> b) -> Result e a -> Result e b
    fmap fx (Ok value) = Ok $ fx value
    fmap fx (Err value) = Err value

instance Applicative (Result e) where
    pure :: a -> Result e a
    pure = Ok

    (<*>) :: Result e (a -> b) -> Result e a -> Result e b
    (<*>) (Ok fx) (Ok value) = Ok $ fx value
    (<*>) (Err value) _ = Err value
    (<*>) _ (Err value) = Err value

instance Monad (Result e) where
    (>>=) :: Result e a -> (a -> Result e b) -> Result e b
    (>>=) (Ok value) fx = fx value
    (>>=) (Err value) fx = Err value