module App.Auth
  ( Token
  , generateToken
  , parseToken
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Utils (decodeBase64, encodeBase64)

type Token = { name :: String, password :: String }

parseToken :: String -> Maybe Token
parseToken s = case split (Pattern ":|:") s of
  [name', password'] ->
    case [decodeBase64 name', decodeBase64 password'] of
      [Just name, Just password] -> Just { name, password }
      _ -> Nothing
  _ -> Nothing

generateToken :: Token -> String
generateToken { name, password } = encodeBase64 (name) <> ":|:" <> encodeBase64 (password)
