module Cloud.Compute.AWS.Lambda (
    runLambda,
    liftLambda,
    runLambdaT,
    liftLambdaT,
    argument,
    context,
    functionName,
    nogood,
    Lambda,
    LambdaT,
    toSerial,
    interop
) where

-- import Data.Default (Default, def)
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


-- newtype Context = Context String
-- instance Default Context where
--     def = "Default Context"

type Lambda ctx evt err a = LambdaT ctx evt err Identity a

runLambda :: Lambda ctx evt err a -> ctx -> evt -> Either err a
runLambda lambda context event = runIdentity (runLambdaT lambda context event)

liftLambda :: a -> Lambda ctx evt err a
liftLambda = liftLambdaT . pure

newtype LambdaT ctx evt err m a = Wrap { unwrap :: ReaderT (evt, ctx) (ExceptT err m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

runLambdaT :: LambdaT ctx evt err m a -> ctx -> evt -> m (Either err a)
runLambdaT lambda c e = ( runExceptT . runReaderT (unwrap lambda) ) (e, c)

liftLambdaT :: (Monad m) => m a -> LambdaT ctx evt err m a
liftLambdaT = Wrap . lift . lift

argument :: (Monad m) => LambdaT ctx evt err m evt
argument = fst <$> Wrap (lift . pure =<< ask)

context :: (Monad m) => LambdaT ctx evt err m ctx
context = snd <$> Wrap (lift . pure =<< ask)

nogood :: (Monad m) => err -> LambdaT ctx evt err m a
nogood = Wrap . lift . throwE

instance MonadTrans (LambdaT ctx evt err) where
    lift = liftLambdaT

-- type AWSLambda i o = i -> IO o

-- type AWSLambdaIntegration = AWSLambda CString CString

interop ::(ByteString -> ByteString -> IO ByteString) -> CString -> CString -> IO CString
interop f context input = do
        context' <- packCString context
        input' <- packCString input
        result' <- f context' input'
        unpackCString result'

toSerial :: (FromJSON input, FromJSON context, ToJSON output, ToJSON error, ToJSON invalid)
  => invalid
  -> (context -> input -> IO (Either output error))
  -> ByteString
  -> ByteString
  -> IO ByteString
toSerial inv f cbytes ibytes = do
    let maybeContext = decodeStrict cbytes
        maybeInput = decodeStrict ibytes
        maybeIOResult = do
            context <- maybeContext
            input <- maybeInput
            pure $ f context input
    case maybeIOResult of
            Just result -> either encodeStrict encodeStrict <$> result
            _ -> pure $ encodeStrict inv

    -- when (isNothing maybeContext)
    --     pure (encodeStrict inv)
    -- when (isNothing maybeInput)
    --     pure (encodeStrict inv)
    -- result <- f (fromJust maybeContext) (fromJust maybeInput)
    -- pure $ either encodeStrict encodeStrict result

toLambda :: (FromJSON evt, FromJSON ctx, ToJSON a, ToJSON err)
    => (forall b. m b -> n b)
    -> LambdaT ctx evt err m a
    -> ctx
    -> evt
    -> n (Either err a)
toLambda interpret handle context event = interpret (runLambdaT handle context event)

demoHandle :: LambdaT String Int String IO [Int]
demoHandle = do
    n <- argument
    when (n > 20) (nogood "number is too big")
    pure [1..n]

demoLambda :: String -> Int -> IO (Either String [Int])
demoLambda = toLambda id demoHandle

demoInterop :: CString -> CString -> IO CString
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

class FunctionInfo m where
    functionName :: m String

instance (Monad m) => FunctionInfo (LambdaT String evt err m) where
    functionName = context
