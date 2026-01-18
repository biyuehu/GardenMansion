module Romi.Core
  ( Guard
  , Handler
  , Route(..)
  , Routes(..)
  , Romi
  , response
  )
  where


import Control.Monad.Error.Class (throwError)
-- import Data.String (Pattern(..), contains)
import Romi.Request (Method, Request)
import Romi.Response (Response)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Effect.Aff (Aff)

type Romi env = ExceptT Response (ReaderT env Aff)

type Guard env a = Request -> Romi env a

type Handler env = Guard env Response

response :: forall env a. Response -> Romi env a
response = throwError

data Route env = Route Method String (Handler env)

type Routes env = Array (Route env)
