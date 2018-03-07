module Cloud.AWS.Lambda (
    toSerial,
    interop,
    toLambda,
    LambdaContext (..)
) where

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Foreign.Marshal.Alloc(free)

import Cloud.Compute (ComputeT, runComputeT)
import Cloud.Compute.Ephemeral (OperationContext (..), TimedOperationContext (..))
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import Data.ByteString (ByteString, packCString, useAsCString)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default (..))
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C (CString, newCString)
import GHC.Generics (Generic)

data LambdaContext = LambdaContext {
        lambdaName :: Text,
        lambdaVersion :: Text,
        lambdaInvocation :: Text,
        lambdaDeadline :: Integer
    } deriving (Eq, Show, Generic)

instance FromJSON LambdaContext

instance Default LambdaContext where
    def = LambdaContext {
        lambdaName = "",
        lambdaVersion = "",
        lambdaInvocation = "",
        lambdaDeadline = 0
     }

instance OperationContext LambdaContext where
    operationName = lambdaName
    operationVersion = lambdaVersion
    operationInvocation = lambdaInvocation

instance TimedOperationContext LambdaContext where
    operationDeadline ctx = posixSecondsToUTCTime $ fromRational (lambdaDeadline ctx % 1000)

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

returned :: IO (IORef CString)
returned = newCString "{}" >>= newIORef

replace :: CString -> IO CString
replace v = do
    ref <- returned
    prev <- atomicModifyIORef' ref (\x -> (v, x))
    free prev
    pure v

-- clear :: IO CString
-- clear = newCString "{}" >>= replace

unpackCString :: ByteString -> IO CString
unpackCString bytes = useAsCString bytes replace

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode

