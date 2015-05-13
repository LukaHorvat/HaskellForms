{-# LANGUAGE TemplateHaskell, DataKinds, KindSignatures, FlexibleInstances #-}
module WinForms.Interface where

import WinForms.Types
import WinForms.Serialization
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import GHC.IO.Handle
import System.IO.Unsafe
import System.Process
import Data.IORef
import Data.List
import Language.Haskell.TH

charLit :: ReadP Char
charLit = readS_to_P readLitChar

stringLit :: ReadP String
stringLit = char '"' *> manyTill charLit (char '"')

word :: ReadP String
word = munch1 (/= ' ')

splitToTokens :: String -> Either String [String]
splitToTokens str
    | null res  = Left $ "Failed to split " ++ str
    | otherwise = Right $ fst $ head res
    where par = many1 $ (stringLit <++ word) <* skipSpaces
          res = filter ((== "") . snd) $ readP_to_S par str

deserializeRead :: Serialize a => String -> Either String a
deserializeRead str = do
    split <- splitToTokens str
    return $ fst $ deserialize split

readResponse :: String -> Either String Response
readResponse = deserializeRead

data Value = Int Int | String String | Color (Vec4 Int) | Point (Vec2 Int) | Size (Vec2 Int) | Font String Float | ObjectId Int
    deriving (Eq, Ord, Show, Read)
data Message = New String | Set Int String Value | Get Int String | Invoke Int String (Int, [Value])
    deriving (Eq, Ord, Show, Read)
data Response = NoResponse | Error String | Value Value
    deriving (Eq, Ord, Show, Read)

instance Serialize (Int, [Value]) where
    serialize (i, vs) = unwords $ [serialize i] ++ map serialize vs
    deserialize rest0 = ((num, map fst res), finalRest)
        where (num, rest1) = deserialize rest0
              res = unfoldr proc (rest1, num)
              finalRest = snd $ last res
              proc (_, 0) = Nothing
              proc (r, n) = Just ((v, r'), (r', n - 1))
                  where (v, r') = deserialize r

deriveSerialize WithHeader ''Value
deriveSerialize WithHeader ''Message
deriveSerialize WithHeader ''Response

class Serialize a => Marshal a where
    toValue   :: a -> Value
    fromValue :: Value -> a

instance Marshal Value where
    toValue   = id
    fromValue = id

data Share = Share { shareId :: Int } deriving (Show, Read, Eq, Ord)
class Marshal a => Shared a where
    newShared :: IO a
    sharedId  :: a -> Int
    fromId    :: Int -> a

instance Serialize Share where
    serialize (Share i) = "ObjectId" ++ show i
    deserialize ("ObjectId" : ss) = (Share i, rest)
        where (i, rest) = deserialize ss
    deserialize x        = error $ "Cannot read ObjectId from " ++ show x

{-
Automatically generates Shared instances fomr arbitrary types of this form

newtype Type = Type Share

with this template

instance Shared Type where
    newShared = fmap Type (I.newShare "Type")
    sharedId (Type s) = I.shareId s
    fromId n = Type (Share n)
-}
deriveShared :: Name -> Q [Dec]
deriveShared typeName = do
    TyConI dec <- reify typeName
    deriveSharedFromDec dec

deriveSharedFromDec :: Dec -> Q [Dec]
deriveSharedFromDec dec = return [InstanceD [] hed [makeNew, makeSharedId, makeFromId]]
    where (NewtypeD _ name _ (NormalC ctor _) _) = dec
          hed = AppT (ConT ''Shared) (ConT name)
          makeFmap =
              NormalB (AppE
                  (AppE (VarE 'fmap) (ConE ctor))
                  (AppE (VarE 'newShare) (LitE (StringL (nameBase ctor)))))
          makeNew = FunD 'newShared [Clause [] makeFmap []]
          makeIdPat = ConP ctor [VarP $ mkName "s"]
          makeIdBody = NormalB (AppE (VarE 'shareId) (VarE $ mkName "s"))
          makeSharedId = FunD 'sharedId [Clause [makeIdPat] makeIdBody []]
          makeFromId =
              FunD 'fromId [Clause [VarP $ mkName "n"]
                (NormalB $ AppE (ConE ctor) (AppE (ConE 'Share) (VarE $ mkName "n"))) []]

newShare :: String -> IO Share
newShare name = do
    Value (ObjectId i) <- sendMessage (New name)
    return $ Share i

property :: (Shared a, Marshal b) => String -> a -> Property (c :: Access) b
property name obj = Property (getProp (sharedId obj) name) (setProp (sharedId obj) name)

data Host = Host { toHostHandle   :: IORef Handle
                 , fromHostHandle :: IORef Handle }
                 deriving Eq

{-# NOINLINE host #-}
host :: Host
host = unsafePerformIO $ do
    r1 <- newIORef $ error "Host not started"
    r2 <- newIORef $ error "Host not started"
    return $ Host r1 r2

startHost :: IO ()
startHost = do
    (Just inH, Just outH, _, _) <- createProcess (proc "C:/Users/Luka/Documents/Haskell/HaskellForms/src/Host/HaskellForms/HaskellForms/bin/Debug/HaskellForms.exe" [])
                                   { std_out = CreatePipe
                                   , std_in  = CreatePipe }
    hSetBuffering inH LineBuffering
    hSetBuffering outH LineBuffering
    writeIORef (toHostHandle host) inH
    writeIORef (fromHostHandle host) outH

sendMessage :: Message -> IO Response
sendMessage msg = do
    toHost <- readIORef (toHostHandle host)
    fromHost <- readIORef (fromHostHandle host)
    hPutStr toHost (serialize msg ++ "\n")
    Right resp <- readResponse <$> hGetLine fromHost
    return resp

getProp :: Marshal a => Int -> String -> IO a
getProp shId propName = do
    (Value v) <- sendMessage (Get shId propName)
    return $ fromValue v

setProp :: Marshal a => Int -> String -> a -> IO ()
setProp shId propName val = do
    _ <- sendMessage (Set shId propName (toValue val))
    return ()

invoke :: Shared a => a -> String -> [Value] -> IO Response
invoke obj methName args = sendMessage (Invoke (sharedId obj) methName (length args, args))
