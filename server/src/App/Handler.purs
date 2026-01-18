module App.Handler where

import Prelude

import App.Auth (generateToken)
import App.Guard (requireAuthAdmin, requireAuthUser, selectChangeingPassword, selectCreatingExpense, selectCreatingMessage, selectCreatingUser, selectDeletingExpense, selectDeletingMessage, selectDeletingUser, selectLogginIn, selectRenaming, selectUpdatingMeta)
import App.Models (DBKey(..), dbOps, expenses, messages, users)
import App.Types (Handler')
import Data.Array (find, length)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), isJust, maybe)
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
  count <- messages.count
  now <- liftEffect now
  messages.insert
    { messageId: count + 1
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
  count <- expenses.count
  now <- liftEffect now
  expenses.insert
    { expenseId: count + 1
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
  pure $ JsonRes $ show $ (map (\{userId, userName, userAlive, userAdmin, userTime} -> { userId, userName, userAlive, userAdmin, userTime }) usersList :: ResUserApi)

createUser :: Handler'
createUser req = do
  _ <- requireAuthAdmin req
  { userName, userPassword, userAlive } <- selectCreatingUser req
  usersList <- users.selectAll
  if isJust (find (\u -> u.userName == userName) usersList) then 
    pure $ errorBadRequest "Username already exists" 
  else do
    now <- liftEffect now
    users.insert
      { userId: length usersList + 1
      , userName
      , userPassword
      , userAlive
      , userAdmin: false
      , userTime: getTime now
      }
    pure $ StatusRes Created

deleteUser :: Handler'
deleteUser req = do
  user <- requireAuthAdmin req
  { deleteUserId, deleteForced } <- selectDeletingUser req
  if deleteUserId == user.userId then
    pure $ errorForbidden "Forbidden to delete administrator user"
  else if deleteForced then do
    users.deleteAll (\u -> u.userId == deleteUserId)
    pure $ StatusRes NoContent
  else do
    users.update (\u -> u.userId == deleteUserId) (\u -> u { userAlive = false })
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
    , infoAlive: user.userAlive
    , infoAdmin: user.userAdmin
    , infoTime: user.userTime
    } :: ResInfoApi)

renameInfo :: Handler'
renameInfo req = do
  user <- requireAuthUser req
  { infoUsername } <- selectRenaming req
  usersList <- users.selectAll
  if isJust (find (\u -> u.userName == infoUsername && u.userId /= user.userId) usersList) then pure $ errorBadRequest "Username already exists"
  else do
    dbOps.put Users $ map (\u -> if u.userId == user.userId then u { userName = infoUsername } else u) usersList
    pure $ StatusRes OK

changePasswordInfo :: Handler'
changePasswordInfo req = do
  user <- requireAuthUser req
  { infoPasswordNew, infoPasswordOld } <- selectChangeingPassword req
  if infoPasswordNew == infoPasswordOld then pure $ errorBadRequest "New password is the same as the old one"
  else do
    usersList <- users.selectAll
    case find (\u -> u.userId == user.userId && u.userPassword == infoPasswordOld) usersList of
      Just _ -> do
        dbOps.put Users $ map (\u -> if u.userId == user.userId then u { userPassword = infoPasswordNew } else u) usersList
        pure $ StatusRes OK
      Nothing -> pure $ errorForbidden "Invalid old password"

loginIn :: Handler'
loginIn req = do
  { loginUsername, loginPassword } <- selectLogginIn req
  usersList <- users.selectAll
  case find (\u -> u.userName == loginUsername && u.userPassword == loginPassword) usersList of
    Just _ -> pure $ JsonRes $ show { "token": generateToken { name: loginUsername, password: loginPassword } }
    Nothing -> pure $ errorUnauthorized "Invalid username or password"