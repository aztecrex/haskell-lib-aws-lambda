module Cloud.Compute.Ephemeral (
    MonadEphemeral (..),
    MonadEphemeralTimer (..)
) where

import Data.Text (Text)
import Data.Time.Clock (DiffTime)

class MonadEphemeral m where
    name :: m Text
    version :: m Text
    invocation :: m Text

class MonadEphemeralTimer m where
    remainingTime :: m DiffTime

