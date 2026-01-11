module App.Auth
  ( Token
  , generateToken
  , parseToken
  , selectAuthUser
  )
  where

import Prelude

import App.Models (users)
import App.Types (State)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Models (ModelUserSingle)
import Romi.Common (Romi)
import Romi.Request (Request, select)
import Utils (decodeBase64, encodeBase64)


type Token = { name :: String, password :: String }

parseToken :: String -> Maybe Token
parseToken s = case split (Pattern ":|:") s of
  [name, password] -> Just { name: decodeBase64 name, password: decodeBase64 password }
  _ -> Nothing

generateToken :: Token -> String
generateToken { name, password } = encodeBase64 (name) <> ":|:" <> encodeBase64 (password)

selectAuthUser :: Request -> Romi State (Either String ModelUserSingle)
selectAuthUser req = case select req.headers "authorization" >>= parseToken of
    Just { name, password } -> do
      user <- users.select (\{userName, userPassword, userAlive} -> userName == name && userPassword == password && userAlive)
      case user of
        Just user' -> pure $ Right user'
        Nothing -> pure $ Left "Invalid credentials"
    Nothing -> pure $ Left "Authorization header not found or invalid format"
