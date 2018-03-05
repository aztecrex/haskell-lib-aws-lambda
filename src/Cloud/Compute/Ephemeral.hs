module Cloud.Compute.Ephemeral (
    MonadOperation (..),
    MonadTimedOperation (..),
    OperationContext (..),
    TimedOperationContext (..),
    MonadClock (..),
    remainingTime
) where

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

import Cloud.Compute (ComputeT, context)

class MonadOperation m where
    name :: m Text
    version :: m Text
    invocation :: m Text

class MonadTimedOperation m where
    deadline :: m UTCTime

class MonadClock m where
    currentTime :: m UTCTime

remainingTime :: (Applicative m, MonadTimedOperation m, MonadClock m) => m NominalDiffTime
remainingTime = diffUTCTime <$> deadline <*> currentTime

class OperationContext a where
    operationName :: a -> Text
    operationVersion :: a -> Text
    operationInvocation :: a -> Text

class TimedOperationContext a where
    operationDeadline :: a -> UTCTime

instance (Monad m, OperationContext ctx) => MonadOperation (ComputeT ctx evt err m) where
    name = operationName <$> context
    version = operationVersion <$> context
    invocation = operationInvocation <$> context

instance (Monad m, TimedOperationContext ctx) => MonadTimedOperation (ComputeT ctx evt err m) where
    deadline = operationDeadline <$> context

instance (Monad m, MonadClock m) => MonadClock (ComputeT ctx evt err m) where
    currentTime = lift currentTime
