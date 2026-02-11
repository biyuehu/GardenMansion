module Romi.Core
  ( Guard
  , Handler
  , Route(..)
  , Routes(..)
  , response
  )
  where


import Control.Monad.Error.Class (throwError, class MonadError)
import Control.Monad.Reader (class MonadReader)
import Romi.Request (Method, Request)
import Romi.Response (class Responseable)

type Guard env a = forall (m :: Type -> Type) e .
                  Responseable e =>
                  MonadError e m =>
                  MonadReader env m =>
                  Request -> m a

type Handler env = forall e . Responseable e => Guard env e

response :: forall env a (m :: Type -> Type) e .
                  Responseable e =>
                  MonadError e m =>
                  MonadReader env m =>
                  e -> m a
response = throwError

data Route env = Route Method String (Handler env)

type Routes env = Array (Route env)
