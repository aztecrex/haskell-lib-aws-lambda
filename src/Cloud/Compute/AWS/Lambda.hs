module Cloud.Compute.AWS.Lambda where

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

type LambdaT evt err m = ReaderT evt (ExceptT err m)

runLambdaT :: LambdaT evt err m a -> evt -> m (Either err a)
runLambdaT lambda = runExceptT . runReaderT lambda

-- newtype LambdaT evt err m a = LambdaT { runLambdaT :: evt -> m (Either err a) }
--     deriving (Functor)

liftLambdaT :: (Monad m) => m a -> LambdaT evt err m a
liftLambdaT = lift . lift

argument :: (Monad m) => LambdaT evt err m evt
argument = ask >>= lift . pure

nogood :: (Monad m) => err -> LambdaT evt err m a
nogood = lift . throwE

-- instance (Applicative m) => Applicative (LambdaT evt err m) where
--     pure = liftLambdaT . pure
--     LambdaT mf <*> LambdaT ma = LambdaT $ (liftA2 . liftA2) (<*>) mf ma

-- -- GeneralizedNewtypeDeriving can't figure out monad
-- instance (Monad m) => Monad (LambdaT evt err m) where
--     (LambdaT r) >>= f = LambdaT $ \evt -> do
--         ea <- r evt
--         case ea of
--             Right a -> runLambdaT (f a) evt
--             Left e -> pure (Left e)

-- instance MonadTrans (LambdaT err evt) where
--     lift = liftLambdaT

