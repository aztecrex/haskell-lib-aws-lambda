module Cloud.Compute.AWS.Lambda (
    runLambda,
    liftLambda,
    runLambdaT,
    liftLambdaT,
    argument,
    nogood,
    Lambda,
    LambdaT,
    toSerial,
    interop
) where

import Data.Functor.Identity(Identity(..), runIdentity)
import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Foreign.Marshal.Alloc(free)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.ByteString (ByteString, packCString, useAsCString)
import Data.ByteString.Lazy (toStrict)
import Foreign.C (CString, newCString)
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)



type Lambda evt err a = LambdaT evt err Identity a

runLambda :: Lambda evt err a -> evt -> Either err a
runLambda lambda event = runIdentity (runLambdaT lambda event)

liftLambda :: a -> Lambda evt err a
liftLambda = liftLambdaT . pure

newtype LambdaT evt err m a = Wrap { unwrap :: ReaderT evt (ExceptT  err m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

runLambdaT :: LambdaT evt err m a -> evt -> m (Either err a)
runLambdaT lambda = runExceptT . runReaderT (unwrap lambda)

liftLambdaT :: (Monad m) => m a -> LambdaT evt err m a
liftLambdaT = Wrap . lift . lift

argument :: (Monad m) => LambdaT evt err m evt
argument = Wrap (lift . pure =<< ask)

nogood :: (Monad m) => err -> LambdaT evt err m a
nogood = Wrap . lift . throwE

instance MonadTrans (LambdaT evt err) where
    lift = liftLambdaT

-- type AWSLambda i o = i -> IO o

-- type AWSLambdaIntegration = AWSLambda CString CString

interop ::(ByteString -> IO ByteString) -> CString -> IO CString
interop f input = packCString input >>= f >>= unpackCString

toSerial :: (FromJSON input, ToJSON output, ToJSON error, ToJSON invalid) => invalid -> (input -> IO (Either output error)) -> ByteString -> IO ByteString
toSerial inv f bytes = do
    let maybeInput = decodeStrict bytes
    case maybeInput of
        Just input -> do
            result <- f input
            pure $ either encodeStrict encodeStrict result
        _ -> pure (encodeStrict inv)

toLambda :: (FromJSON evt, ToJSON a, ToJSON err) => (forall b. m b -> n b) -> LambdaT evt err m a -> evt -> n (Either err a)
toLambda interpret handle event = interpret (runLambdaT handle event)

demoHandle :: LambdaT Int String IO [Int]
demoHandle = do
    n <- argument
    when (n > 20) (nogood "number is too big")
    pure [1..n]

demoLambda :: Int -> IO (Either String [Int])
demoLambda = toLambda id demoHandle

demoInterop :: CString -> IO CString
demoInterop =
    let invalid = "no parse" :: String
    in (interop . toSerial invalid ) demoLambda


returned :: IO (IORef CString)
returned = newCString "{}" >>= newIORef

replace :: CString -> IO CString
replace v = do
    ref <- returned
    prev <- atomicModifyIORef' ref (\x -> (v, x))
    free prev
    pure v

clear :: IO CString
clear = newCString "{}" >>= replace

unpackCString :: ByteString -> IO CString
unpackCString bytes = useAsCString bytes replace

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode

