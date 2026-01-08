module App.Guard where

import Prelude

import App.Auth (selectAuthUser)
import App.Schema (parseLoginInApi, parseReqExpenseApi, parseReqExpenseDeleteApi, parseReqInfoPasswordApi, parseReqInfoRenameApi, parseReqMessageApi, parseReqMessageDeleteApi, parseReqMetaApi, parseReqUserApi, parseReqUserDeleteApi)
import App.Types (GuardState)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Models (ModelUserSingle, ReqExpenseApi, ReqExpenseDeleteApi, ReqInfoPasswordApi, ReqInfoRenameApi, ReqMessageApi, ReqMessageDeleteApi, ReqMetaApi, ReqUserApi, ReqUserDeleteApi, ReqLoginApi)
import Romi.Response (errorForbidden, errorSchema)


authFactory :: forall a. GuardState a -> GuardState { dat :: a, user :: ModelUserSingle }
authFactory f req = do
  result <- f req
  case result of
    Left res -> pure $ Left res
    Right dat -> do
      user <- selectAuthUser req
      case user of
        Left err -> pure $ Left $ errorForbidden err
        Right user' -> pure $ Right { dat, user: user' }


authAdminFactory :: forall a. GuardState a -> GuardState { dat :: a, user :: ModelUserSingle }
authAdminFactory f req = do
  result <- authFactory f req
  case result of
    Left res -> pure $ Left res
    Right { dat, user } -> pure $ if user.userAdmin then Right { dat, user } else Left $ errorForbidden "User is not an admin"

checkLogginIn :: GuardState ReqLoginApi
checkLogginIn req = pure $ lmap errorSchema $ parseLoginInApi req.body

checkCreatingMessage :: GuardState ReqMessageApi
checkCreatingMessage req = pure $ lmap errorSchema $ parseReqMessageApi req.body

checkCreatingExpense :: GuardState ReqExpenseApi
checkCreatingExpense req = pure $ lmap errorSchema $ parseReqExpenseApi req.body

checkRenaming :: GuardState ReqInfoRenameApi
checkRenaming req = pure $ lmap errorSchema $ parseReqInfoRenameApi req.body

checkChangeingPassword :: GuardState ReqInfoPasswordApi
checkChangeingPassword req = pure $ lmap errorSchema $ parseReqInfoPasswordApi req.body

checkUpdatingMeta :: GuardState ReqMetaApi
checkUpdatingMeta req = pure $ lmap errorSchema $ parseReqMetaApi req.body

checkCreatingUser :: GuardState ReqUserApi
checkCreatingUser req = pure $ lmap errorSchema $ parseReqUserApi req.body

checkDeletingUser :: GuardState ReqUserDeleteApi
checkDeletingUser req = pure $ lmap errorSchema $ parseReqUserDeleteApi req.body

checkDeletingMessage :: GuardState ReqMessageDeleteApi
checkDeletingMessage req = pure $ lmap errorSchema $ parseReqMessageDeleteApi req.body

checkDeletingExpense :: GuardState ReqExpenseDeleteApi
checkDeletingExpense req = pure $ lmap errorSchema $ parseReqExpenseDeleteApi req.body
