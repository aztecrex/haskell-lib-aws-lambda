module Cloud.Compute.AWS.Lambda (
    runLambda,
    liftLambda,
    runLambdaT,
    liftLambdaT,
    argument,
    nogood,
    LambdaT
) where

import Data.Functor.Identity(Identity(..), runIdentity)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

type Lambda evt err a = LambdaT evt err Identity a

runLambda :: Lambda evt err a -> evt -> Either err a
runLambda lambda event = runIdentity (runLambdaT lambda event)

liftLambda :: a -> Lambda evt err a
liftLambda v = liftLambdaT (pure v)

newtype LambdaT evt err m a = Wrap { unwrapLambdaT :: ReaderT evt (ExceptT  err m) a }
    deriving (Functor, Applicative, Monad)

runLambdaT :: LambdaT evt err m a -> evt -> m (Either err a)
runLambdaT lambda = runExceptT . runReaderT (unwrapLambdaT lambda)

liftLambdaT :: (Monad m) => m a -> LambdaT evt err m a
liftLambdaT ma = Wrap $ (lift . lift) ma

argument :: (Monad m) => LambdaT evt err m evt
argument = Wrap $ ask >>= lift . pure

nogood :: (Monad m) => err -> LambdaT evt err m a
nogood e = Wrap $ (lift . throwE) e

instance MonadTrans (LambdaT evt err) where
    lift = liftLambdaT
