module App.Handler where

import Prelude

import App.Auth (generateToken, selectAuthUser)
import App.Guard (authAdminFactory, authFactory, checkChangeingPassword, checkCreatingExpense, checkCreatingMessage, checkCreatingUser, checkLogginIn, checkRenaming, checkUpdatingMeta)
import App.Models (DBKey(..), dbm, expenses, messages, users)
import App.Types (HandlerState)
import Data.Array (find)
import Data.Either (isRight)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Romi.Components (guarded)
import Romi.Response (Response(..), Status(..))
import Utils (encodeJson)

fetchAllMessages :: HandlerState
fetchAllMessages _ = do
  messagesList <- messages.selectAll
  pure $ JsonRes $ encodeJson messagesList

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

fetchAllExpenses :: HandlerState
fetchAllExpenses _ = do
  expensesList <- expenses.selectAll
  pure $ JsonRes $ encodeJson expensesList

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

fetchAllUsers :: HandlerState
fetchAllUsers _ = do
  usersList <- users.selectAll
  pure $ JsonRes $ encodeJson usersList

createUser :: HandlerState
createUser = guarded (authAdminFactory checkCreatingUser) $ \_ { dat: { userName, userPassword, userAlive } } -> do
  count <- users.count
  now <- liftEffect now
  users.insert
    { userId: count + 1
    , userName
    , userPassword
    , userAlive
    , userAdmin: false
    , userTime: getTime now
    }
  pure $ StatusRes Created

fetchMeta :: HandlerState
fetchMeta _ = do
  meta <- dbm.get Meta
  pure $ JsonRes $ (maybe "{}" identity) meta

updateMeta :: HandlerState
updateMeta = guarded (authAdminFactory checkUpdatingMeta) $ \_ { dat } -> do
  dbm.put Meta dat
  pure $ StatusRes NoContent

renameInfo :: HandlerState
renameInfo = guarded (authFactory checkRenaming) $ \_ { user, dat: { infoUsername  } } -> do
  usersList <- users.selectAll
  dbm.put Users $ map (\u -> if u.userId == user.userId then user { userName = infoUsername } else u) usersList
  pure $ StatusRes OK

changePasswordInfo:: HandlerState
changePasswordInfo = guarded (authFactory checkChangeingPassword) $ \_ { user, dat: { infoPasswordNew , infoPasswordOld } } -> do
  if infoPasswordNew == infoPasswordOld then pure $ StatusRes BadRequest else do
    usersList <- users.selectAll
    case find (\u -> u.userId == user.userId && u.userPassword == infoPasswordOld) usersList of
      Just _ -> do
        dbm.put Users $ map (\u -> if u.userId == user.userId then user { userPassword = infoPasswordNew } else u) usersList
        pure $ StatusRes OK
      Nothing -> pure $ StatusRes Forbidden

loginIn :: HandlerState
loginIn = guarded checkLogginIn $ \req { loginPassword , loginUsername  } -> do
  user <- selectAuthUser req
  if isRight user then pure $ StatusRes NoContent else do
    usersList <- users.selectAll
    case find (\u -> u.userName == loginUsername && u.userPassword == loginPassword) usersList of
      Just _ -> pure $ JsonRes $ encodeJson { "token": generateToken { name: loginUsername, password: loginPassword } }
      Nothing -> pure $ StatusRes Unauthorized
