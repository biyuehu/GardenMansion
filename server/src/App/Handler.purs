module App.Handler where

import Prelude

import App.Auth (generateToken, selectAuthUser)
import App.Guard (authAdminFactory, authFactory, checkChangeingPassword, checkCreatingExpense, checkCreatingMessage, checkCreatingUser, checkDeletingExpense, checkDeletingMessage, checkDeletingUser, checkLogginIn, checkRenaming, checkUpdatingMeta)
import App.Models (DBKey(..), dbm, expenses, messages, users)
import App.Types (HandlerState)
import Data.Array (find, length)
import Data.Either (Either(..), isRight)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect.Class (liftEffect)
import Models (ResExpenseApi, ResMessageApi, ResUserApi, ResInfoApi)
import Romi.Components (guarded)
import Romi.Response (Response(..), Status(..), errorBadRequest, errorForbidden, errorUnauthorized)

fetchAllMessages :: HandlerState
fetchAllMessages _ = do
  messagesList <- messages.selectAll
  pure $ JsonRes $ show (messagesList :: ResMessageApi)

createMessage :: HandlerState
createMessage = guarded (authFactory checkCreatingMessage) $ \_ { user, dat: { messageText, messageReplyId } } -> do
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

deleteMessage :: HandlerState
deleteMessage = guarded (authFactory checkDeletingMessage) $ \_ { dat: { deleteMessageId } } -> do
  messages.deleteAll (\m -> m.messageId == deleteMessageId)
  pure $ StatusRes NoContent

fetchAllExpenses :: HandlerState
fetchAllExpenses _ = do
  expensesList <- expenses.selectAll
  pure $ JsonRes $ show (expensesList :: ResExpenseApi)

createExpense :: HandlerState
createExpense = guarded (authFactory checkCreatingExpense) $ \_ { user, dat: { expenseAmount, expenseComment  } } -> do
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

deleteExpense :: HandlerState
deleteExpense = guarded (authFactory checkDeletingExpense) $ \_ { dat: { deleteExpenseId } } -> do
  expenses.deleteAll (\e -> e.expenseId == deleteExpenseId)
  pure $ StatusRes NoContent

fetchAllUsers :: HandlerState
fetchAllUsers _ = do
  usersList <- users.selectAll
  pure $ JsonRes $ show $ (map (\{userId, userName, userAlive, userAdmin, userTime} -> { userId, userName, userAlive, userAdmin, userTime }) usersList :: ResUserApi)

createUser :: HandlerState
createUser = guarded (authAdminFactory checkCreatingUser) $ \_ { dat: { userName, userPassword, userAlive } } -> do
  usersList <- users.selectAll
  if isJust (find (\u -> u.userName == userName) usersList) then pure $ errorBadRequest "Username already exists"  else do
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

deleteUser :: HandlerState
deleteUser = guarded (authAdminFactory checkDeletingUser) $ \_ { user, dat: { deleteUserId, deleteForced } } -> do
  if deleteUserId == user.userId then pure $ errorForbidden "Forbidden to delete administrator user" else do
    if deleteForced then do
      users.deleteAll (\u -> u.userId == deleteUserId)
      pure $ StatusRes NoContent
      else do
        users.update (\u -> u.userId == deleteUserId) (\u -> u { userAlive = false })
        pure $ StatusRes NoContent

fetchMeta :: HandlerState
fetchMeta _ = do
  meta <- dbm.get Meta
  pure $ JsonRes $ (maybe "{}" identity) meta

updateMeta :: HandlerState
updateMeta = guarded (authAdminFactory checkUpdatingMeta) $ \_ { dat } -> do
  dbm.put Meta dat
  pure $ StatusRes NoContent

fetchInfo :: HandlerState
fetchInfo req = do
  user <- selectAuthUser req
  case user of
    Left err -> pure $ errorUnauthorized err
    Right {userId: infoId, userName: infoName, userAlive: infoAlive, userAdmin: infoAdmin, userTime: infoTime} -> do
        pure $ JsonRes $ show ({infoId, infoName, infoAlive, infoAdmin, infoTime} :: ResInfoApi)

renameInfo :: HandlerState
renameInfo = guarded (authFactory checkRenaming) $ \_ { user, dat: { infoUsername  } } -> do
  usersList <- users.selectAll
  if isJust (find (\u -> u.userName == infoUsername && u.userId /= user.userId) usersList) then pure $ errorBadRequest "Username already exists" else do
    dbm.put Users $ map (\u -> if u.userId == user.userId then user { userName = infoUsername } else u) usersList
    pure $ StatusRes OK

changePasswordInfo:: HandlerState
changePasswordInfo = guarded (authFactory checkChangeingPassword) $ \_ { user, dat: { infoPasswordNew , infoPasswordOld } } -> do
  if infoPasswordNew == infoPasswordOld then pure $ errorBadRequest "New password is the same as the old one" else do
    usersList <- users.selectAll
    case find (\u -> u.userId == user.userId && u.userPassword == infoPasswordOld) usersList of
      Just _ -> do
        dbm.put Users $ map (\u -> if u.userId == user.userId then user { userPassword = infoPasswordNew } else u) usersList
        pure $ StatusRes OK
      Nothing -> pure $ errorForbidden "Invalid old password"

loginIn :: HandlerState
loginIn = guarded checkLogginIn $ \req { loginPassword , loginUsername  } -> do
  user <- selectAuthUser req
  if isRight user then pure $ StatusRes NoContent else do
    usersList <- users.selectAll
    case find (\u -> u.userName == loginUsername && u.userPassword == loginPassword) usersList of
      Just _ -> pure $ JsonRes $ show { "token": generateToken { name: loginUsername, password: loginPassword } }
      Nothing -> pure $ errorUnauthorized "Invalid username or password"
