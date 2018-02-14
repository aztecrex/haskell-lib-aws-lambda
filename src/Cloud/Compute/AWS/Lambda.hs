module Cloud.Compute.AWS.Lambda where

import Data.Functor.Identity(Identity(..), runIdentity)
import Control.Applicative (liftA2)

type Lambda evt a = LambdaT evt Identity a

runLambda :: Lambda evt a -> evt -> a
runLambda lambda event = runIdentity (runLambdaT lambda event)

liftLambda :: a -> Lambda evt a
liftLambda v = liftLambdaT (pure v)

newtype LambdaT evt m a = LambdaT { runLambdaT :: evt -> m a }
    deriving (Functor)

argument :: (Applicative m) => LambdaT evt m evt
argument = LambdaT pure

liftLambdaT ::  m a -> LambdaT evt m a
liftLambdaT ma = LambdaT $ const ma

instance (Applicative m) => Applicative (LambdaT evt m) where
    pure = liftLambdaT . pure
    LambdaT mf <*> LambdaT ma = LambdaT $ liftA2 (<*>) mf ma

instance (Monad m) => Monad (LambdaT evt m) where
    (LambdaT c) >>= f = LambdaT $ \evt -> do
        a <- c evt
        runLambdaT (f a) evt

