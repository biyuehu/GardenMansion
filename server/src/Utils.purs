module Utils
  ( Schema
  , currentDir
  , decodeBase64
  , encodeBase64
  , encodeSchema
  , endsWith
  , filterMap
  , log'
  , pathJoin
  , schema
  , startsWith
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), foldr)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

foreign import endsWith :: String -> String -> Boolean

foreign import startsWith :: String -> String -> Boolean

filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
filterMap f = foldr (\x acc -> case f x of
    Just y  -> Cons y acc
    Nothing -> acc) Nil

type Schema a = String -> Either String a

showErrors :: List ForeignError -> String
showErrors = foldr (\err acc -> acc <> showError err <> ";") ""
  where
    showError :: ForeignError -> String
    showError (ForeignError err) = err
    showError (TypeMismatch expected actual) = "Expected " <> expected <> " but got " <> actual
    showError (ErrorAtIndex i err) = "Error at index " <> show i <> ": " <> showError err
    showError (ErrorAtProperty prop err) = "Error at property " <> prop <> ": " <> showError err

schema :: forall a. ReadForeign a => Schema a
schema str = case readJSON str of
    Left (NonEmptyList (NonEmpty err a)) -> Left $ showErrors $ Cons err a
    Right x -> Right x

encodeSchema :: forall a. WriteForeign a => a -> String
encodeSchema s = writeJSON s


foreign import currentDir :: String

foreign import pathJoin :: String -> String -> String

log' ∷ ∀ (t24 ∷ Type -> Type). MonadEffect t24 ⇒ String → t24 Unit
log' = liftEffect <<< log

-- foreign import encodeJson :: forall a. a -> String

foreign import encodeBase64 :: String -> String

foreign import decodeBase64 :: String -> String
