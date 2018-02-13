module Cloud.Compute.AWS.Lambda where

newtype LambdaT evt m a = LambdaT { runLambdaT :: evt -> m a }

liftLambdaT :: (Monad m) =>  m a -> LambdaT evt m a
liftLambdaT ma = LambdaT $ const ma
