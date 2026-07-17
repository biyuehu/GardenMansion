module Romi.Server
  ( Server(..)
  , close
  , createServer
  , listen
  , createStaticHandler
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
import Romi.Core (Guard, Route(..), Routes)
import Romi.Request (Method, Request, parseMethod)
import Romi.Response (RequestPrim, Response, ResponsePrim, setResponse, toResponseRaw)

data Server

handlerBus :: forall env. List (Route env) -> Guard env (Maybe Response)
handlerBus routes req = case find match routes of
    Just (Route _ _ handler) -> map Just $ handler req
    Nothing -> pure Nothing
  where
    match (Route method path _) = method == req.method && path == req.path


foreign import createServerPrim :: (Request -> RequestPrim -> ResponsePrim -> Effect Unit) -> (String -> String -> Tuple String String) -> (String -> Method) -> Effect Server

createServer :: forall env. Routes env -> env -> Maybe (RequestPrim -> ResponsePrim -> Effect Unit) -> Aff Server
createServer routes env defaultFn = liftEffect $ createServerPrim (\req reqPrim resPrim ->
  launchAff_ $ do
    result <- runReaderT (runExceptT (handlerBus (
    case fromFoldable routes of
      Just x -> toList x
      Nothing -> Nil
    ) req)) env
    liftEffect $ case case result of
      Left errRes -> Just errRes
      Right res -> res of
      Just res -> setResponse resPrim $ toResponseRaw $ res
      Nothing  -> case defaultFn of
        Just fn -> fn reqPrim resPrim
        Nothing -> pure unit
  ) Tuple parseMethod

foreign import listen :: Server -> Int -> Effect Unit -> Effect Unit

foreign import close :: Server -> Effect Unit

foreign import createStaticHandler :: String -> RequestPrim -> ResponsePrim -> Effect Unit
