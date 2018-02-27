module Cloud.Compute.AWS.Lambda (
    toSerial,
    interop
) where

import Data.Default (Default, def)
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

import Cloud.Compute (ComputeT, runComputeT, abort, event)


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

toLambda :: (FromJSON evt, FromJSON ctx, ToJSON a, ToJSON err)
    => (forall b. m b -> n b)
    -> ComputeT ctx evt err m a
    -> ctx
    -> evt
    -> n (Either err a)
toLambda interpret handle context event = interpret (runComputeT handle context event)

demoHandle :: ComputeT String Int String IO [Int]
demoHandle = do
    n <- event
    when (n > 20) (abort "number is too big")
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

