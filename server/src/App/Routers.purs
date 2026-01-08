module App.Route
  ( routers
  )
  where

import App.Components (logger)
import App.Types (State)
import Romi.Components (Component(..), Components, Rule(..))
import App.Handler (changePasswordInfo, createExpense, createMessage, createUser, fetchAllExpenses, fetchAllMessages, fetchAllUsers, fetchMeta, loginIn, renameInfo, updateMeta)
import Romi.Request (Method(..))

routers :: Components State
routers =
  [ Before Any logger
  , Route GET "/api/messages" fetchAllMessages
  , Route POST "/api/messages" createMessage
  , Route GET "/api/expenses" fetchAllExpenses
  , Route POST "/api/expenses" createExpense
  , Route GET "/api/users" fetchAllUsers
  , Route POST "/api/users" createUser
  , Route GET "/api/meta" fetchMeta
  , Route PUT "/api/meta" updateMeta
  , Route PUT "/api/info/rename" renameInfo
  , Route PUT "/api/info/password" changePasswordInfo
  , Route POST "/api/login" loginIn
  ]