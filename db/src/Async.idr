module Async

%default total

public export
data AsyncIO a = PrivatelyMkAsyncIO

public export
%foreign """
javascript:lambda:(_, a) => async () => a
"""
MkAsyncIO : a -> AsyncIO a

public export
%foreign """
javascript:lambda:(_, f) => async () => f()
"""
MkAsyncIOFromIO : IO a -> AsyncIO a

public export
%foreign """
javascript:lambda:(_, __, f, a) => async () => f(await a())
"""
mapAsyncIO : (a -> b) -> AsyncIO a -> AsyncIO b

public export
%foreign """
javascript:lambda:(_, __, ff, fa) => async () => (await ff())(await fa())
"""
apAsyncIO : AsyncIO (a -> b) -> AsyncIO a -> AsyncIO b

public export
%foreign """
javascript:lambda:(_, __, a, f) => async () => await f(await a())()
"""
bindAsyncIO : AsyncIO a -> (a -> AsyncIO b) -> AsyncIO b

public export
%foreign """
javascript:lambda:(_, f) => () => f().catch((err) => console.error("Idris AsyncIO error:", err))
"""
runAsyncIO : AsyncIO a -> IO ()

public export
Functor AsyncIO where
  map = mapAsyncIO

public export
Applicative AsyncIO where
  pure = MkAsyncIO
  (<*>) = apAsyncIO

public export
Monad AsyncIO where
  (>>=) = bindAsyncIO

