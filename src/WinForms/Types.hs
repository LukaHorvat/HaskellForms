{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveDataTypeable,
             TemplateHaskell, KindSignatures, DataKinds, FlexibleInstances,
             DefaultSignatures, FunctionalDependencies, ConstraintKinds,
             ExistentialQuantification, GADTs #-}
module WinForms.Types where

import GHC.Generics
import Data.Typeable
import WinForms.Serialization

data Vec2 a = Vec2 a a
    deriving (Eq, Ord, Show, Read, Functor, Typeable, Generic)

data Vec4 a = Vec4 a a a a
    deriving (Eq, Ord, Show, Read, Functor, Typeable, Generic)

deriveSerialize WithoutHeader ''Vec2
deriveSerialize WithoutHeader ''Vec4

{-
instance Functor (Property where
    fmap f (Property getter _) = Property (fmap f getter) undefined

instance Applicative (Property Gettable) where
    pure x = Property (pure x) undefined
    Property ioF _ <*> Property ioA _ = Property (ioF <*> ioA) undefined

instance Monad (Property Gettable) where
    return x = Property (return x) undefined
    Property ioA _ >>= f = Property io undefined
        where io = do a <- ioA
                      let (Property ioB _) = f a
                      ioB
-}

data Property a where
    Property :: IO a -> (a -> IO ()) -> Property a

data Event a b = Event Int deriving (Show, Read, Eq, Ord)

{-
The following classes classify types that can be recovered from properties, depending if the
properties are gettable and/or settable.
For example, we can recofer an `IO a` from a `Property a` if that property is gettable.

Bindings of various properties don't return `Property` objects but polymorfic values that belong
to these classes. This serves as a conveinence because it means that the property will assume
the needed type depending on how it's used. For example

  main :: IO ()
  main = do
      form <- newForm
      clr  <- backColor form

Here, the `backColor` function doesn't return `IO Color`, but `FromGettableSettable f => f Color`,
but since there's an `instance FromGettableSettable IO`, it just assumes that type.

On the other hand

  main :: IO ()
  main = do
      form <- newForm
      backColor form #= Color (Vec4 255 0 0 255)

Now it assumes the type of `Setter Color` because, again, there's an instance for it.
-}
class FromGettableSettable f where
    getSet :: Property a -> f a

instance FromGettableSettable Property where
    getSet (Property getter setter) = Property getter setter

instance FromGettableSettable IO where
    getSet (Property getter _) = getter

data Setter a where
    Setter :: (a -> IO ()) -> Setter a

instance FromGettableSettable Setter where
    getSet (Property _ setter) = Setter setter

class FromGettableSettable f => FromGettable f where
    get :: Property a -> f a
    default get :: FromGettableSettable f => Property a -> f a
    get = getSet

instance FromGettable IO
instance FromGettable Property
