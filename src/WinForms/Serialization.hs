{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module WinForms.Serialization where

import Language.Haskell.TH
import Data.List

class Serialize a where
    serialize :: a -> String
    deserialize :: [String] -> (a, [String])

simpleDeserialize :: Read a => String -> [String] -> (a, [String])
simpleDeserialize _    (s : ss) = (read s, ss)
simpleDeserialize name x        = error $ "Cannot read type " ++ name ++ " from " ++ show x

instance Serialize Int where
    serialize = show
    deserialize = simpleDeserialize "Int"

instance Serialize Float where
    serialize = show
    deserialize = simpleDeserialize "Float"

instance Serialize String where
    serialize = show
    deserialize (s : ss) = (s, ss)
    deserialize x        = error $ "Cannot read type String from " ++ show x

instance Serialize Bool where
    serialize True = "true"
    serialize False = "false"
    deserialize ("true" : ss)  = (True, ss)
    deserialize ("false" : ss) = (False, ss)
    deserialize x              = error $ "Cannot read Bool from " ++ show x

{-
Automatically generates Serialize instances fomr arbitrary types using this form

instance (Serialize tva1, Serialize tvar2..) => Serialize (T tvar1 tvar2..) where
    serialize (Cons1 arg1 arg2..) = unwords ["Cons1", serialize arg1, ...]
    serialize (Cons2 arg1 arg2..) = unwords ["Cons2", serialize arg1, ...]
    ...
    deserialize ("Cons1" : rest0) = (Cons1 arg1 arg2.., restN)
        where (arg1, rest1) = deserialize rest0
              (arg2, rest2) = deserialize rest1
              ...
    deserialize ("Cons2" : rest0) = (Cons2 arg1 arg2.., restN)
        where (arg1, rest1) = deserialize rest0
              (arg2, rest2) = deserialize rest1
              ...
    ...
-}
data SerializationType = WithHeader | WithoutHeader deriving (Show, Read, Eq, Ord)

deriveSerialize :: SerializationType -> Name -> Q [Dec]
deriveSerialize serType typeName = do
    TyConI (DataD _ name (map (\(PlainTV x) -> x) -> typeVars) cons _) <- reify typeName
    let ctx = map (ClassP ''Serialize . return . VarT) typeVars
    let hed = AppT (ConT ''Serialize) (foldl AppT (ConT name) (map VarT typeVars))
    let makeSerPat (NormalC cName (length -> n)) =
            ConP cName $ map (\i -> VarP $ mkName ("arg" ++ show i)) [1..n]
    let varES n = map (\i -> VarE $ mkName ("arg" ++ show i)) [1..n]
    let varApps n = map (AppE (VarE 'serialize)) $ varES n
    let makeUnwords (NormalC cName (length -> n)) =
            NormalB (AppE (VarE 'unwords) (ListE $
                (if serType == WithHeader then [LitE (StringL $ nameBase cName)] else [])
                ++ varApps n))
    let makeSerialize con = Clause [makeSerPat con] (makeUnwords con) []
    let makeDeserPat (NormalC cName _) =
            (if serType == WithHeader then InfixP (LitP $ StringL $ nameBase cName) '(:) else id)
            (VarP $ mkName "rest0")
    let makePair (NormalC cName (length -> n)) =
            NormalB $ TupE [foldl' AppE (ConE cName) $ varES n, VarE $ mkName $ "rest" ++ show n]
    let makeWhere i =
            ValD (TupP [VarP $ mkName $ "arg" ++ show i, VarP $ mkName $ "rest" ++ show i])
                 (NormalB $ AppE (VarE 'deserialize) $ VarE $ mkName $ "rest" ++ show (i - 1)) []
    let makeDeserialize con@(NormalC _ (length -> n)) =
            Clause [makeDeserPat con] (makePair con) (map makeWhere [1..n])
    return [InstanceD ctx hed
               [ FunD 'serialize (map makeSerialize cons)
               , FunD 'deserialize (map makeDeserialize cons)]]
