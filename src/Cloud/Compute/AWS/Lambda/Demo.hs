module Cloud.Compute.AWS.Lambda.Demo where

import Control.Monad (when)

import Cloud.Compute (ComputeT, abort, event)
import Cloud.Compute.AWS.Lambda (toSerial, interop, toLambda)

import Foreign.C (CString)



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

