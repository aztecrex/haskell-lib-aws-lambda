module Cloud.Compute.Ephemeral (
    MonadEphemeral (..),
    MonadEphemeralTimer (..)
) where

import Data.Text (Text)
import Data.Time.Clock (DiffTime, UTCTime)

class MonadEphemeral m where
    name :: m Text
    version :: m Text
    invocation :: m Text

class MonadEphemeralTimer m where
    remainingTime :: m DiffTime

class Ephemeral a where
    functionName :: a -> Text
    functionVersion :: a -> Text
    funtionInvocation :: a -> Text

class EphemeralTimer a where
    functionTimeLeft :: a -> UTCTime -> DiffTime


