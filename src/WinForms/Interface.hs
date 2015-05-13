{-# LANGUAGE TemplateHaskell, DataKinds, KindSignatures, FlexibleInstances, ExistentialQuantification #-}
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
import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

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

readIncomming :: String -> Either String Incomming
readIncomming = deserializeRead

data Value = Int Int | String String | Color (Vec4 Int)
           | Point (Vec2 Int) | Size (Vec2 Int) | Font String Float | ObjectId Int
           deriving (Eq, Ord, Show, Read)
data Message = New String | Set Int String Value | Get Int String
             | Invoke Int String (Int, [Value]) | GetEvent Int String
    deriving (Eq, Ord, Show, Read)
data Response = NoResponse | Error String | Value Value
    deriving (Eq, Ord, Show, Read)
data EventMsg = TargetArgs Int Value Value
    deriving (Eq, Ord, Show, Read)
data Incomming = EventMsg EventMsg | Response Response
    deriving (Eq, Ord, Show, Read)

instance Serialize (Int, [Value]) where
    serialize (i, vs) = unwords $ serialize i : map serialize vs
    deserialize rest0 = ((num, map fst res), finalRest)
        where (num, rest1) = deserialize rest0
              res = unfoldr alg (rest1, num)
              finalRest = snd $ last res
              alg (_, 0) = Nothing
              alg (r, n) = Just ((v, r'), (r', n - 1))
                  where (v, r') = deserialize r

deriveSerialize WithHeader ''Value
deriveSerialize WithHeader ''Message
deriveSerialize WithHeader ''Response
deriveSerialize WithHeader ''EventMsg
deriveSerialize WithHeader ''Incomming

class Serialize a => Marshal a where
    toValue   :: a -> Value
    fromValue :: Value -> a

instance Marshal Value where
    toValue   = id
    fromValue = id

instance Marshal String where
    toValue = String
    fromValue (String s) = s
    fromValue _ = error "Value not a String"

instance Marshal Int where
    toValue = Int
    fromValue (Int n) = n
    fromValue _ = error "Value not an Int"

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

data Handler = forall a b. (Marshal a, Marshal b) => Handler (a -> b -> IO ())

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
                 , fromHostHandle :: IORef Handle
                 , responseChan   :: Chan Response
                 , eventHandlers  :: IORef (Map Int Handler) }
                 deriving Eq

{-# NOINLINE host #-}
host :: Host
host = unsafePerformIO $ do
    r1 <- newIORef $ error "Host not started"
    r2 <- newIORef $ error "Host not started"
    r3 <- newIORef Map.empty
    ch1 <- newChan
    return $ Host r1 r2 ch1 r3

startHost :: Bool -> IO ()
startHost debug = do
    (Just inH, Just outH, _, _) <- createProcess (proc "C:/Users/Luka/Documents/Haskell/HaskellForms/src/Host/HaskellForms/HaskellForms/bin/Debug/HaskellForms.exe" [])
                                   { std_out = CreatePipe
                                   , std_in  = CreatePipe }
    hSetBuffering inH LineBuffering
    hSetBuffering outH LineBuffering
    writeIORef (toHostHandle host) inH
    writeIORef (fromHostHandle host) outH
    void $ forkIO $ listen debug

listen :: Bool -> IO ()
listen debug = do
    fromHost <- readIORef (fromHostHandle host)
    forever $ do
        Right inc <- readIncomming <$> hGetLine fromHost
        when debug $ print inc
        case inc of
            EventMsg (TargetArgs evtId tgt arg)  -> do
                handlers <- readIORef $ eventHandlers host
                case Map.lookup evtId handlers of
                    Just (Handler f) -> void $ forkIO $ f (fromValue tgt) (fromValue arg) --Maybe a bad idea? Not sure.
                    Nothing          -> return ()
            Response resp -> writeChan respCh resp
    where respCh  = responseChan host

sendMessage :: Message -> IO Response
sendMessage msg = do
    toHost <- readIORef (toHostHandle host)
    hPutStr toHost (serialize msg ++ "\n")
    readChan $ responseChan host

getProp :: Marshal a => Int -> String -> IO a
getProp shId propName = do
    (Value v) <- sendMessage (Get shId propName)
    return $ fromValue v

setProp :: Marshal a => Int -> String -> a -> IO ()
setProp shId propName val = do
    _ <- sendMessage (Set shId propName (toValue val))
    return ()

invoke :: Shared a => String -> [Value] -> a -> IO Response
invoke methName args obj = sendMessage (Invoke (sharedId obj) methName (length args, args))

invokeMarshal :: (Shared a, Marshal b) => String -> [Value] -> a -> IO b
invokeMarshal methName args obj = do
    Value v <- invoke methName args obj
    return $ fromValue v

invokeVoid :: Shared a => String -> [Value] -> a -> IO ()
invokeVoid methName args obj = do
    NoResponse <- invoke methName args obj
    return ()

event :: (Shared a, Marshal b, Marshal c) => String -> a -> IO (Event b c)
event name x = do
    Value (ObjectId i) <- sendMessage (GetEvent (sharedId x) name)
    return $ Event i

addHandler :: (Marshal a, Marshal b) => Event a b -> (a -> b -> IO ()) -> IO ()
addHandler (Event evtId) h = modifyIORef (eventHandlers host) (Map.insert evtId (Handler h))
