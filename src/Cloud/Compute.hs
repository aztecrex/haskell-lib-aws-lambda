module Cloud.Compute (
    Compute,
    runCompute,
    ComputeT,
    runComputeT,
    MonadCompute (..),
) where

import Data.Functor.Identity (Identity, runIdentity)
import Control.Applicative (Applicative, pure)
import Control.Monad (Monad)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.IO.Class (MonadIO)

type Compute ctx evt err a = ComputeT ctx evt err Identity a

runCompute :: Compute ctx evt err a -> ctx -> evt -> Either err a
runCompute computation context event = runIdentity (runComputeT computation context event)

newtype ComputeT ctx evt err m a = Wrap { unwrap :: ReaderT (evt, ctx) (ExceptT err m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ComputeT ctx evt err) where
    lift = liftComputeT

runComputeT :: ComputeT ctx evt err m a -> ctx -> evt -> m (Either err a)
runComputeT lambda c e = ( runExceptT . runReaderT (unwrap lambda) ) (e, c)

liftComputeT :: (Monad m) => m a -> ComputeT ctx evt err m a
liftComputeT = Wrap . lift . lift

class MonadCompute ctx evt err m | m -> err ctx evt where
    event :: m evt
    context :: m ctx
    abort :: forall a. err -> m ()

instance (Monad m) => MonadCompute ctx evt err (ComputeT ctx evt  err m) where
    event = fst <$> Wrap (lift . pure =<< ask)
    context = snd <$> Wrap (lift . pure =<< ask)
    abort = Wrap . lift . throwE
