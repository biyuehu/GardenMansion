module App.Types
  ( AfterHandlerState
  , BeforeHandlerState
  , GuardState
  , HandlerState
  , State(..)
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Models (ResUserSingle)
import Romi.Components (AfterHandler, BeforeHandler, Handler, Guard)
import Romi.Db (class ProvideDB, DB)

newtype State = State
  { user :: Maybe ResUserSingle
  , db :: DB
  }

instance ProvideDB State where
  getDB (State { db, user:_ }) = pure db

type BeforeHandlerState = BeforeHandler State

type HandlerState = Handler State

type AfterHandlerState = AfterHandler State

type GuardState b = Guard State b
