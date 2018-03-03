module Cloud.Compute.Ephemeral (
    MonadEphemeralInfo (..),
    MonadEphemeralTimer (..),
    EphemeralInfo (..)
) where

import Data.Text (Text)
import Data.Time.Clock (DiffTime, {-  UTCTime -} )

import Cloud.Compute (ComputeT, context)

class MonadEphemeralInfo m where
    name :: m Text
    version :: m Text
    invocation :: m Text

class MonadEphemeralTimer m where
    remainingTime :: m DiffTime

class EphemeralInfo a where
    functionName :: a -> Text

-- class EphemeralTimer a where
--     functionTimeLeft :: a -> UTCTime -> DiffTime


instance (Monad m, EphemeralInfo ctx) => MonadEphemeralInfo (ComputeT ctx evt err m) where
    name = functionName <$> context
    version = error "not implemented"
    invocation = error "not implemented"

