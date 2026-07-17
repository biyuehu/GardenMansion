module Utils
  ( currentDir
  , decodeBase64
  , encodeBase64
  , endsWith
  , filterMap
  , log'
  , pathJoin
  , readDhallFile
  , startsWith
  )
  where

import Prelude

import App.Schema (parseConfig)
import App.Types (ApplicationConfig)
import Data.Either (Either(..))
import Data.List (List(..), foldr)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

foreign import endsWith :: String -> String -> Boolean

foreign import startsWith :: String -> String -> Boolean

filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
filterMap f = foldr (\x acc -> case f x of
    Just y  -> Cons y acc
    Nothing -> acc) Nil

foreign import currentDir :: String

foreign import pathJoin :: String -> String -> String

log' ∷ ∀ (t24 ∷ Type -> Type). MonadEffect t24 ⇒ String → t24 Unit
log' = liftEffect <<< log

-- foreign import encodeJson :: forall a. a -> String

foreign import encodeBase64 :: String -> String

foreign import decodeBase64Prim :: String -> (String -> Maybe String) -> Maybe String -> Maybe String

decodeBase64 :: String -> Maybe String
decodeBase64 str = decodeBase64Prim str Just Nothing

foreign import readDhallFilePrim :: String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)

readDhallFile :: String -> Effect (Either String ApplicationConfig)
readDhallFile filePath = do
  rawResult <- readDhallFilePrim filePath Left Right
  pure case rawResult of
    Left err -> Left err
    Right json ->
      case parseConfig json of
        Left decodeErrs ->
          Left $ "Config decode failed for \"" <> filePath <> "\": " <> show decodeErrs
        Right cfg ->
          Right cfg
