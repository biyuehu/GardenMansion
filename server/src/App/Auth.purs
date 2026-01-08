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


foreign import decodeStr :: String -> String

foreign import encodeStr :: String -> String

type Token = { name :: String, password :: String }

parseToken :: String -> Maybe Token
parseToken s = case split (Pattern ":|:") s of
  [name, password] -> Just { name: decodeStr name, password: decodeStr password }
  _ -> Nothing

generateToken :: Token -> String
generateToken { name, password } = encodeStr (name) <> ":|:" <> encodeStr (password)

selectAuthUser :: Request -> Romi State (Either String ModelUserSingle)
selectAuthUser req = case select req.headers "Authorization" >>= parseToken of
  Just { name, password } -> do
    user <- users.select (\{userName, userPassword, userAlive} -> userName == name && userPassword == password && userAlive)
    case user of
      Just user' -> pure $ Right user'
      Nothing -> pure $ Left "Invalid credentials"
  Nothing -> pure $ Left "Authorization header not found"
