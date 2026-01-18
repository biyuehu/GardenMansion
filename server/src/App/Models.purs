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
import Romi.Db (ListModel, DBOps, createModel, dbOpsOf)

data DBKey = Users | Expenses | Messages | Meta

instance Show DBKey where
  show Users = "users"
  show Expenses = "expenses"
  show Messages = "messages"
  show Meta = "meta"

dbOps :: DBOps DBKey
dbOps = dbOpsOf

type ListModel' a = ListModel Env a

users :: ListModel' ModelUserSingle
users = createModel
  { key: Users
  , parse: parseModelUsers
  }

messages :: ListModel' ResMessageSingle
messages = createModel
  { key: Messages
  , parse: parseModelMessages
  }

expenses :: ListModel' ResExpenseSingle
expenses = createModel
  { key: Expenses
  , parse: parseModelExpenses
  }
