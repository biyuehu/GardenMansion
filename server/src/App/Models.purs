module App.Models
  ( DBKey(..)
  , dbOps
  , expenses
  , messages
  , userIsAccesible
  , userIsAdmin
  , users
  )
  where

import Prelude

import App.Schema (parseModelExpenses, parseModelMessages, parseModelUsers)
import App.Types (Env)
import Models (ModelUserSingle, ResMessageSingle, ResExpenseSingle)
import Romi.Core (Romi)
import Romi.Db (DBOps, ListModel, makeModel, dbOpsOf)

data DBKey = Users | Expenses | Messages | Meta

instance Show DBKey where
  show Users = "users"
  show Expenses = "expenses"
  show Messages = "messages"
  show Meta = "meta"

dbOps :: DBOps (Romi Env) DBKey
dbOps = dbOpsOf

type ListModel' a = ListModel (Romi Env) a

users :: ListModel' ModelUserSingle
users = makeModel
  { key: Users
  , parse: parseModelUsers
  , rowId: \{userId} -> userId
  }

messages :: ListModel' ResMessageSingle
messages = makeModel
  { key: Messages
  , parse: parseModelMessages
  , rowId: \{messageId} -> messageId
  }

expenses :: ListModel' ResExpenseSingle
expenses = makeModel
  { key: Expenses
  , parse: parseModelExpenses
  , rowId: \{expenseId} -> expenseId
  }

userIsAccesible :: ModelUserSingle -> Boolean
userIsAccesible user@{ userLevel  } = userLevel == 0 || userIsAdmin user

userIsAdmin :: ModelUserSingle -> Boolean
userIsAdmin { userLevel  } = userLevel == 1