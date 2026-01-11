module App.Models
  ( DBKey(..)
  , dbm
  , expenses
  , messages
  , users
  )
  where

import Prelude

import App.Schema (parseModelExpenses, parseModelMessages, parseModelUsers)
import App.Types (State)
import Models (ModelUserSingle, ResMessageSingle, ResExpenseSingle)
import Romi.Db (DBM, ListModel, createModel, dbmOf)

data DBKey = Users | Expenses | Messages | Meta

instance Show DBKey where
  show Users = "users"
  show Expenses = "expenses"
  show Messages = "messages"
  show Meta = "meta"

dbm :: DBM DBKey
dbm = dbmOf

type ListModel' a = ListModel State a

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
