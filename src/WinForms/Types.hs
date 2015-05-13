{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveDataTypeable, TemplateHaskell, KindSignatures, DataKinds #-}
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

data Property (access :: Access) a = Property (IO a) (a -> IO ())
data Access = Gettable | GettableSettable deriving (Show, Read, Eq, Ord)

data Event a b = Event Int deriving (Show, Read, Eq, Ord)
