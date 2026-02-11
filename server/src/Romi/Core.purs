module Romi.Core
  ( Guard
  , Handler
  , Romi
  , Route(..)
  , Routes(..)
  , response
  )
  where


import Control.Monad.Error.Class (throwError, class MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT)
import Effect.Aff (Aff)
import Romi.Request (Method, Request)
import Romi.Response (class Responseable, Response)

type Romi env = ExceptT Response (ReaderT env Aff)

type Guard env a = Request -> Romi env a

type Handler env = Guard env Response

response :: forall env a (m :: Type -> Type) e .
                  Responseable e =>
                  MonadError e m =>
                  MonadReader env m =>
                  e -> m a
response = throwError

data Route env = Route Method String (Handler env)

type Routes env = Array (Route env)
