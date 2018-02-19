# Haskell Library for AWS Lambda

Convenient model for AWS Lambda Functions.

## Experiments

### LambdaT transformer

I have been experimenting with the shape. The most aesthetic to me so far is
a `Reader` + `Except` ( `event -> m (Either error result)` ). This is encoded in the
`LambdaT` type.

The module contains functions to convert a LambdaT or corresponding handler
into a function suitable for export, `CString -> IO CString`. These functions expect the
event, error, and result types to be instances of `FromJSON`, `ToJSON`, and `ToJSON` respectively.

### Context

The shape will support context, I think, and that's what I'll work on next. The addition
of context will make the computation something like
`(event, Context) -> Either error result` . One thing I will experiment with is
exposing Context directly vs functions that close over it. For example, in
IO, Context could support a function that returns the remaining time to perform the
computation. I am thinking that function might be more conveniently exposed as
a simple effect in the monad rather than requiring the user to obtain the context
and pass it to the function.

### Template Haskell

Original ideas for TH did not work out well. I've come up with a slightly different
plan that I'll work on after finishing the computation model.

Say you have a named computation, `cloudWork :: LambdaT Input Error IO Output` . Using
TH, I am hoping you can do something like `makeLambda 'cloudWork` and end up with a named,
exported function like `cloudWork_export :: CString -> IO CString` . `makeLambda` would
also export the new function name to a file where it could be used when generating interop
code in the host language.


### Module Names and Hackage Categories

Not sure what those should be yet.


