module App.Guard where

import Prelude

import App.Auth (parseToken)
import App.Models (users)
import App.Schema (parseLoginInApi, parseReqExpenseApi, parseReqExpenseDeleteApi, parseReqInfoPasswordApi, parseReqInfoRenameApi, parseReqMessageApi, parseReqMessageDeleteApi, parseReqMetaApi, parseReqUserApi, parseReqUserDeleteApi)
import App.Types (Guard')
import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Models (ModelUserSingle, ReqExpenseApi, ReqExpenseDeleteApi, ReqInfoPasswordApi, ReqInfoRenameApi, ReqLoginApi, ReqMessageApi, ReqMetaApi, ReqUserApi, ReqUserDeleteApi, ReqMessageDeleteApi)
import Romi.Core (response)
import Romi.Request (select)
import Romi.Response (errorForbidden, errorSchema)



requireAuthUser :: Guard' ModelUserSingle
requireAuthUser req = case select req.headers "authorization" >>= parseToken of
  Just { name, password } -> do
    user <- users.select (\{userName, userPassword, userAlive} -> userName == name && userPassword == password && userAlive)
    case user of
      Just user' -> pure user'
      Nothing -> response $ errorForbidden "Invalid credentials"
  Nothing -> response $ errorForbidden "Authorization header not found or invalid format"



requireAuthAdmin :: Guard' ModelUserSingle
requireAuthAdmin req = do
  user <- requireAuthUser req
  if user.userAdmin then pure user else response $ errorForbidden "User is not an admin"

selectLogginIn :: Guard' ReqLoginApi
selectLogginIn req = except $ lmap errorSchema $ parseLoginInApi req.body

selectCreatingMessage :: Guard' ReqMessageApi
selectCreatingMessage req = except $ lmap errorSchema $ parseReqMessageApi req.body

selectCreatingExpense :: Guard' ReqExpenseApi
selectCreatingExpense req = except $ lmap errorSchema $ parseReqExpenseApi req.body

selectRenaming :: Guard' ReqInfoRenameApi
selectRenaming req = except $ lmap errorSchema $ parseReqInfoRenameApi req.body

selectChangeingPassword :: Guard' ReqInfoPasswordApi
selectChangeingPassword req = except $ lmap errorSchema $ parseReqInfoPasswordApi req.body

selectUpdatingMeta :: Guard' ReqMetaApi
selectUpdatingMeta req = except $ lmap errorSchema $ parseReqMetaApi req.body

selectCreatingUser :: Guard' ReqUserApi
selectCreatingUser req = except $ lmap errorSchema $ parseReqUserApi req.body

selectDeletingUser :: Guard' ReqUserDeleteApi
selectDeletingUser req = except $ lmap errorSchema $ parseReqUserDeleteApi req.body

selectDeletingMessage :: Guard' ReqMessageDeleteApi
selectDeletingMessage req = except $ lmap errorSchema $ parseReqMessageDeleteApi req.body

selectDeletingExpense :: Guard' ReqExpenseDeleteApi
selectDeletingExpense req = except $ lmap errorSchema $ parseReqExpenseDeleteApi req.body
