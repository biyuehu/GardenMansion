module App.Handler where

import Prelude

import App.Guard (authAdminFactory, authFactory, checkCreatingExpense, checkCreatingMessage, checkCreatingUser)
import App.Models (DBKey(..), dbm, expenses, messages, users)
import App.Types (HandlerState)
import Data.JSDate (getTime, now)
import Data.Maybe (maybe)
import Effect.Class (liftEffect)
import Romi.Components (guarded)
import Romi.Response (Response(..), Status(..))

fetchAllMessages :: HandlerState
fetchAllMessages _ = do
  messagesList <- messages.selectAll
  pure $ JsonRes $ show messagesList

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
  pure $ JsonRes $ show expensesList

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
  pure $ JsonRes $ show usersList


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

-- createMeta :: HandlerState
-- createMeta = guarded (authAdminFactory checkCreatingUser) $ \_ { dat: { metaValue } } -> do
--   dbm.set Meta metaValue
--   pure $ StatusRes Created
