module App.Constant where

import Models (ModelUsers)

defaultModelUsers :: ModelUsers
defaultModelUsers = [
  { userId: 1
  , userName: "admin"
  , userNickname: "Romisama"
  , userPassword: "123456"
  -- , userEmail: "admin@gmail.com"
  , userTime: 0.0
  , userLevel: 1
  }
]

dbPrefix :: String
dbPrefix = "garden_mansion_"

