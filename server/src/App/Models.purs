module App.Models
  ( DBKey(..)
  , dbOps
  , expenses
  , messages
  , users
  )
  where

import Prelude

import App.Schema (parseModelExpenses, parseModelMessages, parseModelUsers)
import App.Types (Env)
import Models (ModelUserSingle, ResMessageSingle, ResExpenseSingle)
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
  }

messages :: ListModel' ResMessageSingle
messages = makeModel
  { key: Messages
  , parse: parseModelMessages
  }

expenses :: ListModel' ResExpenseSingle
expenses = makeModel
  { key: Expenses
  , parse: parseModelExpenses
  }
