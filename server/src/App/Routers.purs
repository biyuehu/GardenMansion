module App.Route
  ( routers
  )
  where

import App.Components (logger)
import App.Handler (changePasswordInfo, createExpense, createMessage, createUser, deleteExpense, deleteMessage, deleteUser, fetchAllExpenses, fetchAllMessages, fetchAllUsers, fetchInfo, fetchMeta, loginIn, updateMeta)
import App.Types (State)
import Romi.Components (Component(..), Components, Rule(..))
import Romi.Request (Method(..))

routers :: Components State
routers =
  [ Before Any logger
  , Route GET "/api/messages" fetchAllMessages
  , Route POST "/api/messages" createMessage
  , Route DELETE "/api/messages" deleteMessage
  , Route GET "/api/expenses" fetchAllExpenses
  , Route POST "/api/expenses" createExpense
  , Route DELETE "/api/expenses" deleteExpense
  , Route GET "/api/users" fetchAllUsers
  , Route POST "/api/users" createUser
  , Route DELETE "/api/users" deleteUser
  , Route GET "/api/meta" fetchMeta
  , Route PUT "/api/meta" updateMeta
  , Route GET "/api/info" fetchInfo
  -- , Route PUT "/api/info/rename" renameInfo
  , Route PUT "/api/info/password" changePasswordInfo
  , Route POST "/api/login" loginIn
  ]
