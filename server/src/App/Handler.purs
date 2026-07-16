module App.Handler where

import Prelude

import App.Auth (generateToken)
import App.Guard (requireAuthAdmin, requireAuthUser, selectChangeingPassword, selectCreatingExpense, selectCreatingMessage, selectCreatingUser, selectDeletingExpense, selectDeletingMessage, selectDeletingUser, selectLogginIn, selectRenaming, selectUpdatingMeta, tryAuthUser)
import App.Models (DBKey(..), dbOps, expenses, messages, userIsAccesible, users)
import App.Schema (parseModelMeta)
import App.Types (Env(..), Handler')
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Array (find, foldl, length, null)
import Data.Either (Either(..))
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String (take)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Models (ResExpenseApi, ResMessageApi, ResUserApi, ResInfoApi)
import Romi.Logger (Logger, error, info)
import Romi.Request (Request)
import Romi.Response (Response(..), Status(..), errorBadRequest, errorForbidden, errorUnauthorized)
import Simple.JSON (writeJSON)

logEntry :: Logger Aff -> String -> Maybe Request -> Aff Unit
logEntry logger name reqMaybe =
  info logger $ "<cyan>[" <> name <> "]</cyan> started" <>
    maybe "" (\r -> " " <> toStr r) reqMaybe
  where
    toStr req =
      "method: " <> show req.method <>
      ", path: " <> req.path <>
      (if null req.query then "" else ", query: " <> show req.query) <>
      (if req.body /= "" then ", body: " <> take 200 req.body else "")

logSuccess :: Logger Aff -> String -> String -> Aff Unit
logSuccess logger name msg =
  info logger $ "<green>[" <> name <> "]</green> succeeded" <>
    (if msg /= "" then " – " <> msg else "")

logError :: Logger Aff -> String -> String -> Aff Unit
logError logger name msg =
  error logger $ "<red>[" <> name <> "]</red> error: " <> msg

fetchAllMessages :: Handler'
fetchAllMessages req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "fetchAllMessages" (Just req)
  _ <- requireAuthUser req
  messagesList <- messages.selectAll
  let count = length messagesList
  liftAff $ logSuccess logger "fetchAllMessages" ("returned " <> show count <> " messages")
  pure $ JsonRes $ writeJSON (messagesList :: ResMessageApi)

createMessage :: Handler'
createMessage req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "createMessage" (Just req)
  user <- requireAuthUser req
  { messageText, messageReplyId } <- selectCreatingMessage req
  liftAff $ info logger ("User <yellow>" <> show user.userId <> "</yellow> creating message: '" <> messageText <> "'" <>
    maybe "" (\rid -> " (reply to #" <> show rid <> ")") messageReplyId)
  messageId <- messages.rowId
  now <- liftEffect now
  messages.insert
    { messageId
    , messageText
    , messageUserId: user.userId
    , messageReplyId
    , messageReleaseTime: getTime now
    }
  liftAff $ logSuccess logger "createMessage" ("message #" <> show messageId <> " created")
  pure $ StatusRes Created

deleteMessage :: Handler'
deleteMessage req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "deleteMessage" (Just req)
  _ <- requireAuthAdmin req
  { deleteMessageId } <- selectDeletingMessage req
  liftAff $ info logger ("Deleting message #" <> show deleteMessageId)
  messages.deleteAll (\m -> m.messageId == deleteMessageId)
  liftAff $ logSuccess logger "deleteMessage" ("message #" <> show deleteMessageId <> " deleted")
  pure $ StatusRes NoContent

fetchAllExpenses :: Handler'
fetchAllExpenses req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "fetchAllExpenses" (Just req)
  expensesList <- expenses.selectAll
  let total = foldl (\acc e -> acc + e.expenseAmount) 0.0 expensesList
  liftAff $ logSuccess logger "fetchAllExpenses" ("returned " <> show (length expensesList) <> " items, total amount: " <> show total)
  pure $ JsonRes $ writeJSON (expensesList :: ResExpenseApi)

createExpense :: Handler'
createExpense req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "createExpense" (Just req)
  user <- requireAuthAdmin req
  { expenseAmount, expenseComment } <- selectCreatingExpense req
  liftAff $ info logger ("User <yellow>" <> show user.userId <> "</yellow> creating expense: " <> show expenseAmount <> " (" <> expenseComment <> ")")
  expenseId <- expenses.rowId
  now <- liftEffect now
  expenses.insert
    { expenseId
    , expenseAmount
    , expenseComment
    , expenseUserId: user.userId
    , expenseTime: getTime now
    }
  liftAff $ logSuccess logger "createExpense" ("expense #" <> show expenseId <> " created")
  pure $ StatusRes Created

deleteExpense :: Handler'
deleteExpense req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "deleteExpense" (Just req)
  _ <- requireAuthAdmin req
  { deleteExpenseId } <- selectDeletingExpense req
  liftAff $ info logger ("Deleting expense #" <> show deleteExpenseId)
  expenses.deleteAll (\e -> e.expenseId == deleteExpenseId)
  liftAff $ logSuccess logger "deleteExpense" ("expense #" <> show deleteExpenseId <> " deleted")
  pure $ StatusRes NoContent

fetchAllUsers :: Handler'
fetchAllUsers req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "fetchAllUsers" (Just req)
  usersList <- users.selectAll
  let count = length usersList
  liftAff $ logSuccess logger "fetchAllUsers" ("returned " <> show count <> " users")
  pure $ JsonRes $ writeJSON $ (map (\{userId, userName, userNickname,userLevel, userTime} -> { userId, userName, userNickname, userLevel, userTime }) usersList :: ResUserApi)

createUser :: Handler'
createUser req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "createUser" (Just req)
  _ <- requireAuthAdmin req
  { userName, userPassword, userNickname } <- selectCreatingUser req
  liftAff $ info logger ("Creating user with name: " <> userName <> ", nickname: " <> userNickname)
  usersList <- users.selectAll

  when (isJust $ find (\u -> u.userName == userName) usersList) $ do
    liftAff $ logError logger "createUser" ("Username already exists: " <> userName)
    throwError $ errorBadRequest "Username already exists"

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
  liftAff $ logSuccess logger "createUser" ("user #" <> show userId <> " created")
  pure $ StatusRes Created

deleteUser :: Handler'
deleteUser req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "deleteUser" (Just req)
  user <- requireAuthAdmin req
  { deleteUserId, deleteForced } <- selectDeletingUser req
  liftAff $ info logger ("Admin <yellow>" <> show user.userId <> "</yellow> deleting user #" <> show deleteUserId <> (if deleteForced then " (forced)" else " (soft)"))

  when (deleteUserId == user.userId) $ do
    liftAff $ logError logger "deleteUser" "Attempt to delete self"
    throwError $ errorForbidden "Forbidden to delete yourself"

  if deleteForced then do
    users.deleteAll (\u -> u.userId == deleteUserId)
    liftAff $ logSuccess logger "deleteUser" ("user #" <> show deleteUserId <> " permanently deleted")
    pure $ StatusRes NoContent
  else do
    users.update (\u -> u.userId == deleteUserId) (\u -> u { userLevel = -1 })
    liftAff $ logSuccess logger "deleteUser" ("user #" <> show deleteUserId <> " banned")
    pure $ StatusRes NoContent

fetchMeta :: Handler'
fetchMeta req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "fetchMeta" (Just req)
  isAuthUser <- tryAuthUser req >>= (\user -> pure $ isJust user)
  meta <- dbOps.get Meta
  liftAff $ logSuccess logger "fetchMeta" ("meta data " <> (if isJust meta then "found" else "not found"))
  pure $ JsonRes $ (maybe "{}" (\meta' ->
    case parseModelMeta meta' of
      Left _ -> "{}"
      Right meta'' -> writeJSON $ if isAuthUser then meta'' else meta'' { webNotice = "" }
  )) meta

updateMeta :: Handler'
updateMeta req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "updateMeta" (Just req)
  _ <- requireAuthAdmin req
  dat <- selectUpdatingMeta req
  liftAff $ info logger ("Updating meta: " <> show dat)
  dbOps.put Meta dat
  liftAff $ logSuccess logger "updateMeta" "meta updated"
  pure $ StatusRes NoContent

fetchInfo :: Handler'
fetchInfo req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "fetchInfo" (Just req)
  user <- requireAuthUser req
  liftAff $ logSuccess logger "fetchInfo" ("info for user #" <> show user.userId)
  pure $ JsonRes $ writeJSON
    ({ infoId: user.userId
    , infoName: user.userName
    , infoNickname: user.userNickname
    , infoLevel: user.userLevel
    , infoTime: user.userTime
    } :: ResInfoApi)

renameInfo :: Handler'
renameInfo req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "renameInfo" (Just req)
  { userId } <- requireAuthUser req
  { infoUsername } <- selectRenaming req
  liftAff $ info logger ("User <yellow>" <> show userId <> "</yellow> renaming nickname to: " <> infoUsername)
  usersList <- users.selectAll

  when (isJust $ find (\u -> u.userName == infoUsername && u.userId /= userId) usersList) $ do
    liftAff $ logError logger "renameInfo" ("Nickname already taken: " <> infoUsername)
    throwError $ errorBadRequest "Username already exists"

  dbOps.put Users $ map (\u -> if u.userId == userId then u { userNickname = infoUsername } else u) usersList
  liftAff $ logSuccess logger "renameInfo" ("nickname changed to " <> infoUsername)
  pure $ StatusRes OK

changePasswordInfo :: Handler'
changePasswordInfo req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "changePasswordInfo" (Just req)
  { userId } <- requireAuthUser req
  { infoPasswordNew, infoPasswordOld } <- selectChangeingPassword req
  liftAff $ info logger ("User <yellow>" <> show userId <> "</yellow> changing password")

  when (infoPasswordNew == infoPasswordOld) $ do
    liftAff $ logError logger "changePasswordInfo" "New password same as old"
    throwError $ errorBadRequest "New password is the same as the old one"

  usersList <- users.selectAll

  when (isNothing $ find (\u -> u.userId == userId && u.userPassword == infoPasswordOld) usersList) $ do
    liftAff $ logError logger "changePasswordInfo" "Invalid old password"
    throwError $ errorForbidden "Invalid old password"
  dbOps.put Users $ map (\u -> if u.userId == userId then u { userPassword = infoPasswordNew } else u) usersList

  liftAff $ logSuccess logger "changePasswordInfo" "password changed"
  pure $ StatusRes OK

loginIn :: Handler'
loginIn req = do
  Env { logger } <- ask
  liftAff $ logEntry logger "loginIn" (Just req)
  { loginUsername, loginPassword } <- selectLogginIn req
  liftAff $ info logger ("Login attempt for user: " <> loginUsername)
  usersList <- users.selectAll
  case find (\u -> u.userName == loginUsername && u.userPassword == loginPassword) usersList of
    Just user -> do
      when (not $ userIsAccesible user) $ do
        liftAff $ logError logger "loginIn" ("User banned: " <> loginUsername)
        throwError $ errorForbidden "User had been banned"
      let token = generateToken { name: loginUsername, password: loginPassword }
      liftAff $ logSuccess logger "loginIn" ("user " <> loginUsername <> " logged in, token issued")
      pure $ JsonRes $ writeJSON { "token": token }
    Nothing -> do
      liftAff $ logError logger "loginIn" ("Invalid credentials for user: " <> loginUsername)
      throwError $ errorUnauthorized "Invalid username or password"
