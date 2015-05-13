{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
module WinForms.Controls.Base where

import WinForms.Types
import WinForms.Serialization
import Language.Haskell.TH
import WinForms.Interface (Shared, Share(..), deriveSharedFromDec, newShared, Marshal, Value(ObjectId))
import qualified WinForms.Interface as I

data Font = Font String Float
    deriving (Eq, Ord, Show, Read)
data Size = Size (Vec2 Int)
    deriving (Eq, Ord, Show, Read)
data Point = Point (Vec2 Int)
    deriving (Eq, Ord, Show, Read)
data Color = Color (Vec4 Int)
    deriving (Eq, Ord, Show, Read)

deriveSerialize WithHeader ''Font
deriveSerialize WithHeader ''Point
deriveSerialize WithHeader ''Size
deriveSerialize WithHeader ''Color

color :: Int -> Int -> Int -> Int -> Color
color r g b a = Color (Vec4 r g b a)

font :: String -> Float -> Font
font = Font

point :: Int -> Int -> Point
point x y = Point (Vec2 x y)

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

instance Marshal Color where
    toValue (Color x)     = I.Color x
    fromValue (I.Color x) = Color x
    fromValue _ = error "Value not a Font"

{-
Makes a new Shared type with the following template

newtype Name = Name Share deriving (Read, Show, Eq, Ord)
instance Serialize Name where
    serialize (Name s) = serialize s
    deserialize ss = let (sh, rest) = deserialize ss in (Name sh, rest)
instance Marshal Name where
    toValue (Name (Share n)) = ObjectId n
    fromValue (ObjectId n) = Name (Share n)
    fromValue _ = error "Value not an ObjectId"
deriveShared ''Name
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
    marshal <- [d|
        instance Marshal $(conT name) where
            toValue $(conP name [conP 'Share [varP n]]) = ObjectId $(varE n)
            fromValue (ObjectId i) = $(conE name) (Share i)
            fromValue _ = error "Value not an ObjectId"
        |]
    return $ [typeDec] ++ serial ++ marshal ++ shared
    where name = mkName str
          typeDec = NewtypeD [] name [] (NormalC name [(NotStrict,ConT ''Share)])
                             [''Read, ''Show, ''Eq, ''Ord]
          n = mkName "n"
          s = mkName "s"

{-
Adds these declarations for the type

newName :: IO Name
newName = I.newShared
-}
makeInstantiable :: String -> Q [Dec]
makeInstantiable str = do
    shared <- makeShared str
    def <- [d|$(varP $ newN) = newShared|]
    return $ shared ++ [sig] ++ def
    where sig = SigD newN (AppT (ConT ''IO) (ConT name))
          newN = mkName $ "new" ++ str
          name = mkName str
