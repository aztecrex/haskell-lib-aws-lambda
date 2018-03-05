# Haskell Library for AWS Lambda

Convenient model for AWS Lambda Functions.

## Cloud.Compute Module

### ComputeT

This transformer encodes a function from an event and a context that can fail. It is
implemented as Reader + Except. The reader context is a `(context, event)`.

`runComputeT (ComputeT context event error m a)` accepts `context` and `event` as separate arguments and produces 
`Either error a` .

A convenience type `Compute event error a` just bolts `ComputeT` onto `Identity`.

### MonadCompute class

`MonadCompute ctx evt err m` provides access to the event and context:

`event :: m evt`
`context :: m ctx`

And provides a way to fail:

`abort :: err -> m ()`

## Cloud.Compute.Ephemeral Module

`MonadOperation m` provides context information:

`name :: m Text` : name of the running operation
`version :: m Text` : version of the running operation
`invocation :: m Text` : invocation identifier of the running operation

`MonadTimedOperation m` provides access to  running operation's deadline:

`deadline :: m UTCTime`

`MonadOperation` is automatically defined for `ComputeT` if the context is a
`OperationContext` instance.

Similarly, `MonadTimedOperation` is automatically defined for `ComputeT` if the context
is a `TimedOperationContext` instance.

`MonadClock m` exposes the current time:

`currentTime :: m UTCTime`

Any computation that is an instance of `MonadClock` and `MonadTimedOPeration` can provide
the remaining time to complete:

`remainingTime :: m NominalDiffTime`


## Cloud.AWS.Lambda Module

A `ComputeT` can automatically become an AWS Lambda function if its context, event, and error types
are `FromJSON`, `FromJSON`, and `ToJSON`respectively. See `toSerial` and related functions
for information.

The module also defines a context type, `LambdaContext` that is an instance of `OperationContext` .
Eventually, this may become the only context type supported for AWS Lambda.

## Template Haskell

Original ideas for TH did not work out well. I've come up with a slightly different
plan that I'll work on after finishing the computation model.

Say you have a named computation, `cloudWork :: LambdaT Input Error IO Output` . Using
TH, I am hoping you can do something like `makeLambda 'cloudWork` and end up with a named,
exported function like `cloudWork_export :: CString -> IO CString` . `makeLambda` would
also export the new function name to a file where it could be used when generating interop
code in the host language.


## Module Names and Hackage Categories

Not sure what those should be yet.


