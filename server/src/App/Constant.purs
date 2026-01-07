module App.Constant where

import Prelude

import Models (ModelMeta, ModelUsers)
import Utils (currentDir, pathJoin)

defaultServerPort :: Int
defaultServerPort = 8080

defaultModelUsers :: ModelUsers
defaultModelUsers = [
  { userId: 1
  , userName: "admin"
  , userPassword: "123456"
  -- , userEmail: "admin@gmail.com"
  , userTime: 0.0
  , userAlive: true
  , userAdmin: true
  }
]

defaultModelMeta :: ModelMeta
defaultModelMeta =
  { webUrl: "http://localhost:" <> show defaultServerPort
  , webName: "Garden Mansion"
  , webTitle: "Garden Mansion"
  , webNotice: "Here's a notice for the Garden Mansion"
  , webStartTime: 1.0
  }

dbDirectory :: String
dbDirectory = pathJoin currentDir "db"

dbPrefix :: String
dbPrefix = "garden_mansion_"

