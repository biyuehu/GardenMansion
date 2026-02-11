module App.Handler where

import Prelude

import App.Auth (generateToken)
import App.Guard (requireAuthAdmin, requireAuthUser, selectChangeingPassword, selectCreatingExpense, selectCreatingMessage, selectCreatingUser, selectDeletingExpense, selectDeletingMessage, selectDeletingUser, selectLogginIn, selectRenaming, selectUpdatingMeta)
import App.Models (DBKey(..), dbOps, expenses, messages, userIsAccesible, users)
import App.Types (Handler')
import Control.Monad.Error.Class (throwError)
import Data.Array (find)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Effect.Class (liftEffect)
import Models (ResExpenseApi, ResMessageApi, ResUserApi, ResInfoApi)
import Romi.Response (Response(..), Status(..), errorBadRequest, errorForbidden, errorUnauthorized)

fetchAllMessages :: Handler'
fetchAllMessages _ = do
  messagesList <- messages.selectAll
  pure $ JsonRes $ show (messagesList :: ResMessageApi)

createMessage :: Handler'
createMessage req = do
  user <- requireAuthUser req
  { messageText, messageReplyId } <- selectCreatingMessage req
  messageId <- messages.rowId
  now <- liftEffect now
  messages.insert
    { messageId
    , messageText
    , messageUserId: user.userId
    , messageReplyId
    , messageReleaseTime: getTime now
    }
  pure $ StatusRes Created

deleteMessage :: Handler'
deleteMessage req = do
  _ <- requireAuthAdmin req
  { deleteMessageId } <- selectDeletingMessage req
  messages.deleteAll (\m -> m.messageId == deleteMessageId)
  pure $ StatusRes NoContent

fetchAllExpenses :: Handler'
fetchAllExpenses _ = do
  expensesList <- expenses.selectAll
  pure $ JsonRes $ show (expensesList :: ResExpenseApi)

createExpense :: Handler'
createExpense req = do
  user <- requireAuthAdmin req
  { expenseAmount, expenseComment } <- selectCreatingExpense req
  expenseId <- expenses.rowId
  now <- liftEffect now
  expenses.insert
    { expenseId
    , expenseAmount
    , expenseComment
    , expenseUserId: user.userId
    , expenseTime: getTime now
    }
  pure $ StatusRes Created

deleteExpense :: Handler'
deleteExpense req = do
  _ <- requireAuthAdmin req
  { deleteExpenseId } <- selectDeletingExpense req
  expenses.deleteAll (\e -> e.expenseId == deleteExpenseId)
  pure $ StatusRes NoContent

fetchAllUsers :: Handler'
fetchAllUsers _ = do
  usersList <- users.selectAll
  pure $ JsonRes $ show $ (map (\{userId, userName, userNickname,userLevel, userTime} -> { userId, userName, userNickname, userLevel, userTime }) usersList :: ResUserApi)

createUser :: Handler'
createUser req = do
  _ <- requireAuthAdmin req
  { userName, userPassword, userNickname } <- selectCreatingUser req
  usersList <- users.selectAll

  when (isJust $ find (\u -> u.userName == userName) usersList) $ throwError $ errorBadRequest "Username already exists"

  userId <- users.rowId
  now <- liftEffect now
  users.insert
    { userId
    , userName
    , userNickname
    , userPassword
    , userLevel: 0
    , userTime: getTime now
    }
  pure $ StatusRes Created

deleteUser :: Handler'
deleteUser req = do
  user <- requireAuthAdmin req
  { deleteUserId, deleteForced } <- selectDeletingUser req

  when (deleteUserId == user.userId) $ throwError $ errorForbidden "Forbidden to delete yourself"

  if deleteForced then do
    users.deleteAll (\u -> u.userId == deleteUserId)
    pure $ StatusRes NoContent
  else do
    users.update (\u -> u.userId == deleteUserId) (\u -> u { userLevel = -1 })
    pure $ StatusRes NoContent

fetchMeta :: Handler'
fetchMeta _ = do
  meta <- dbOps.get Meta
  pure $ JsonRes $ (maybe "{}" identity) meta

updateMeta :: Handler'
updateMeta req = do
  _ <- requireAuthAdmin req
  dat <- selectUpdatingMeta req
  dbOps.put Meta dat
  pure $ StatusRes NoContent

fetchInfo :: Handler'
fetchInfo req = do
  user <- requireAuthUser req
  pure $ JsonRes $ show
    ({ infoId: user.userId
    , infoName: user.userName
    , infoNickname: user.userNickname
    , infoLevel: user.userLevel
    , infoTime: user.userTime
    } :: ResInfoApi)

renameInfo :: Handler'
renameInfo req = do
  { userId } <- requireAuthUser req
  { infoUsername } <- selectRenaming req
  usersList <- users.selectAll

  when (isJust $ find (\u -> u.userName == infoUsername && u.userId /= userId) usersList) $ throwError $ errorBadRequest "Username already exists"

  dbOps.put Users $ map (\u -> if u.userId == userId then u { userNickname = infoUsername } else u) usersList
  pure $ StatusRes OK

changePasswordInfo :: Handler'
changePasswordInfo req = do
  { userId } <- requireAuthUser req
  { infoPasswordNew, infoPasswordOld } <- selectChangeingPassword req

  when (infoPasswordNew == infoPasswordOld) $ throwError $ errorBadRequest "New password is the same as the old one"

  usersList <- users.selectAll

  when (isNothing $ find (\u -> u.userId == userId && u.userPassword == infoPasswordOld) usersList) $ throwError $ errorForbidden "Invalid old password"

  pure $ StatusRes OK

loginIn :: Handler'
loginIn req = do
  { loginUsername, loginPassword } <- selectLogginIn req
  usersList <- users.selectAll
  case find (\u -> u.userName == loginUsername && u.userPassword == loginPassword) usersList of
    Just user -> do
      when (not $ userIsAccesible user) $ throwError $ errorForbidden "User had been banned"
      pure $ JsonRes $ show { "token": generateToken { name: loginUsername, password: loginPassword } }
    Nothing -> throwError $ errorUnauthorized "Invalid username or password"
