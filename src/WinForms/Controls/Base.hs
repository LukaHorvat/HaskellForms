{-# LANGUAGE TemplateHaskell, DefaultSignatures, MultiParamTypeClasses,
             FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
module WinForms.Controls.Base where

import WinForms.Types
import WinForms.Serialization
import Language.Haskell.TH
import WinForms.Interface (Shared, Share(..), deriveSharedFromDec, Instantiable
                          , newInstantiable, newShare, Marshal, Value(ObjectId)
                          , unsafeCast)
import qualified WinForms.Interface as I

data Font = Font String Float
    deriving (Eq, Ord, Show, Read)
data Size = Size (Vec2 Int)
    deriving (Eq, Ord, Show, Read)
data Point = Point (Vec2 Int)
    deriving (Eq, Ord, Show, Read)
data PointF = PointF (Vec2 Float)
    deriving (Eq, Ord, Show, Read)
data Color = Color (Vec4 Int)
    deriving (Eq, Ord, Show, Read)

deriveSerialize WithHeader ''Font
deriveSerialize WithHeader ''Point
deriveSerialize WithHeader ''Size
deriveSerialize WithHeader ''Color
deriveSerialize WithHeader ''PointF

color :: Int -> Int -> Int -> Int -> Color
color r g b a = Color (Vec4 r g b a)

font :: String -> Float -> Font
font = Font

point :: Int -> Int -> Point
point x y = Point (Vec2 x y)

pointF :: Float -> Float -> PointF
pointF x y = PointF (Vec2 x y)

size :: Int -> Int -> Size
size x y = Size (Vec2 x y)

instance Marshal Font where
    toValue (Font s f)     = I.Font s f
    fromValue (I.Font s f) = Font s f
    fromValue _ = error "Value not a Font"

instance Marshal Size where
    toValue (Size x)     = I.Size x
    fromValue (I.Size x) = Size x
    fromValue _ = error "Value not a Size"

instance Marshal Point where
    toValue (Point x)     = I.Point x
    fromValue (I.Point x) = Point x
    fromValue _ = error "Value not a Point"

instance Marshal PointF where
    toValue (PointF x)     = I.PointF x
    fromValue (I.PointF x) = PointF x
    fromValue _ = error "Value not a PointF"

instance Marshal Color where
    toValue (Color x)     = I.Color x
    fromValue (I.Color x) = Color x
    fromValue _ = error "Value not a Font"

nullRef :: Share
nullRef = Share (-1)

{-
Makes a new Shared type with the following template

newtype Name = Name Share deriving (Read, Show, Eq, Ord)
instance Serialize Name where
    serialize (Name s) = serialize s
    deserialize ss = let (sh, rest) = deserialize ss in (Name sh, rest)
deriveShared ''Name
instance Marshal Name where
-}
makeShared :: String -> Q [Dec]
makeShared str = do
    shared <- deriveSharedFromDec typeDec
    serial <- [d|
        instance Serialize $(conT name) where
            serialize $(conP name [varP s]) = serialize $(varE s)
            deserialize ss = ($(conE name) sh, rest)
                where (sh, rest) = deserialize ss
        |]
    marshal <- [d|instance Marshal $(conT name) where|]
    return $ [typeDec] ++ serial ++ shared ++ marshal
    where name = mkName str
          typeDec = NewtypeD [] name [] (NormalC name [(NotStrict,ConT ''Share)])
                             [''Read, ''Show, ''Eq, ''Ord]
          n = mkName "n"
          s = mkName "s"

{-
Adds these declarations for the type

instance Instantiable Name where
    newInstantiable = fmap Name (newShare "Name")

newName :: IO Name
newName = newInstantiable
-}
makeInstantiable :: String -> Q [Dec]
makeInstantiable str = do
    shared <- makeShared str
    inst   <- [d|
        instance Instantiable $(conT name) where
            newInstantiable = fmap $(conE name) (newShare $(litE $ stringL $ nameBase name))
        |]
    def <- [d|$(varP $ newN) = newInstantiable|]
    return $ shared ++ inst ++ [sig] ++ def
    where sig = SigD newN (AppT (ConT ''IO) (ConT name))
          newN = mkName $ "new" ++ str
          name = mkName str

{-
Support for using instances of a subclass as instances of a superclass
-}
class Subclass a b where
    cast :: b -> a
    default cast :: (Shared a, Shared b) => b -> a
    cast = unsafeCast

instance Subclass a a where
    cast = id
