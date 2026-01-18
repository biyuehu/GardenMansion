module Romi.Server
  ( Server(..)
  , close
  , createServer
  , listen
  )
  where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.List (List(..), find)
import Data.List.NonEmpty (fromFoldable)
import Data.List.Types (toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Romi.Core (Route(..), Routes, Handler)
import Romi.Request (Method, Request, parseMethod)
import Romi.Response (ResponsePrim, errorNotFound, setResponse, toResponseRaw)

data Server

handlerBus :: forall env. List (Route env) -> Maybe (Handler env) -> Handler env
handlerBus routes default req = case find match routes of
    Just (Route _ _ handler) -> handler req
    Nothing -> case default of
        Just defHandler -> defHandler req
        Nothing         -> pure $ errorNotFound "Not found"
  where
    match (Route method path _) = method == req.method && path == req.path


foreign import createServerPrim :: (Request -> ResponsePrim -> Effect Unit) -> (String -> String -> Tuple String String) -> (String -> Method) -> Effect Server

createServer :: forall env. Routes env -> env -> Maybe (Handler env) -> Aff Server
createServer routes env default = liftEffect $ createServerPrim (\req resPrim ->
  launchAff_ $ do
    result <- runReaderT (runExceptT (handlerBus (
    case fromFoldable routes of
        Just x -> toList x
        Nothing -> Nil
    ) default req)) env
    liftEffect $ setResponse resPrim $ toResponseRaw $ case result of
      Left errRes -> errRes
      Right res -> res
  ) Tuple parseMethod

foreign import listen :: Server -> Int -> Effect Unit -> Effect Unit

foreign import close :: Server -> Effect Unit
