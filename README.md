# Haskell Library for Ephemeral Functions

Convenient model for ephemeral functions including AWS's function as a service
offering, Lambda.

## Cloud.Compute Module

### ComputeT

The `ComputeT` transformer encapsulates a function from an event and a context allowing
for failure. It is essentially a Reader and Except stack where the Reader context is
`(context, event)` for some computation context and input event provided by an integrated
compute infrastructure, such as AWS Lambda.

`runComputeT (ComputeT ctx evt err m a)` accepts context (type `ctx`) and event
(type `evt`)  as separate arguments and produces `m (Either err a)`. `ComputeT` is an
instance of `MonadTrans` for construction from other monad stacks.

A convenience type alias `Compute context event error a` bolts `ComputeT` onto `Identity`.
It can be run with `runCompute` to produce a simple `Either error a`.

### MonadCompute class

`MonadCompute ctx evt err m` provides access to the event and context:

```Haskell
event :: m evt
context :: m ctx
```

And provides a way to fail:

```Haskell
abort :: err -> m ()
```

`ComputeT ctx evt err m` is an instance of the class.

## Cloud.Compute.Ephemeral Module

`MonadOperation m` provides context information from the integrated compute infrastructure:

```Haskell
name :: m Text       -- name of the running operation
version :: m Text    -- version of the running operation
invocation :: m Text -- invocation identifier of the running operation
```

`MonadTimedOperation m` provides access to an operation's deadline if supported by the
compute infrastructure:

```Haskell
deadline :: m UTCTime   -- when the operation is scheduled to terminate
```

`MonadOperation` is automatically defined for `ComputeT` if the context is a
`OperationContext` instance.

Similarly, `MonadTimedOperation` is automatically defined for `ComputeT` if the context
is a `TimedOperationContext` instance.

`MonadClock m` exposes the current time:

```Haskell
currentTime :: m UTCTime
```

`ComputeT ctx evt err m` is an instance of `MonadClock` if `m` is itself an instance.

Any computation that is an instance of `MonadClock` and `MonadTimedOperation` can determine
the amount of time remaining to the operation:

```Haskell
remainingTime :: (MonadClock m, MonadTimedOperation m) => m NominalDiffTime
```

## Cloud.AWS.Lambda Module

## toSerial and Friends

A `ComputeT` can automatically become an AWS Lambda function if its context, event, error, and
encapsulated types are `FromJSON`, `FromJSON`, `ToJSON`, and `ToJSON`respectively. See `toSerial`
and related functions for information.

The converted function has an FFI signature:

```Haskell
CString -> CString -> IO CString
```

To integrate it with AWS Lambda, export the produced function. When built as a shared
object for Linux (`.so`), it can be called from a supported host environment such a
Python though the Python FFI.

## LambdaContext

The module also defines a context type, `LambdaContext` that is an instance of `OperationContext`
Eventually, this may become the only context type supported for AWS Lambda. The next steps are
to add support for timed operations by making `LambdaContext` and instance of
`TimedOperationContext`.

## Template Haskell

Original ideas for TH did not work out well. I've come up with a slightly different
plan that I'll work on after finishing the computation model.

Say you have a named computation, `cloudWork :: ComputeT Context Input Error IO Output` . Using
TH, I am hoping you can do something like `makeLambda 'cloudWork` and end up with a named,
exported function like `cloudWork_export :: CString -> CString -> IO CString` . `makeLambda` would
also export the new function name to a file where it could be used when generating interop
code in the host language.

## Module Names and Hackage Categories

Not sure what those should be yet.
