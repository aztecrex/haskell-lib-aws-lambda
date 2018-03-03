module Cloud.Compute.Ephemeral (
    MonadEphemeralInfo (..),
    MonadEphemeralTimer (..),
    EphemeralInfo (..),
    MonadClock (..),
    CountDown (..)
) where

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)


import Cloud.Compute (ComputeT, context)

class MonadEphemeralInfo m where
    name :: m Text
    version :: m Text
    invocation :: m Text

class MonadEphemeralTimer m where
    remainingTime :: m NominalDiffTime

class EphemeralInfo a where
    functionName :: a -> Text
    functionVersion :: a -> Text
    functionInvocation :: a -> Text

class MonadClock m where
    currentTime :: m UTCTime

class CountDown a where
    deadline :: a -> UTCTime

instance (Monad m, EphemeralInfo ctx) => MonadEphemeralInfo (ComputeT ctx evt err m) where
    name = functionName <$> context
    version = functionVersion <$> context
    invocation = functionInvocation <$> context

instance (Monad m, MonadClock m, CountDown ctx) => MonadEphemeralTimer (ComputeT ctx evt err m) where
    remainingTime = do
        now <- lift currentTime
        ctx <- context
        pure $ diffUTCTime (deadline ctx) now

