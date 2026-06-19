module App.Types
  ( Env(..)
  , Guard'
  , Handler'
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Models (ResUserSingle)
import Romi.Core (Handler, Guard)
import Romi.Db (class ProvideDB, DB)
import Romi.Logger (Logger)

newtype Env = Env
  { user :: Maybe ResUserSingle
  , db :: DB
  , logger :: Logger Aff
  }

instance ProvideDB Env where
  getDB (Env { db, user:_ }) = pure db

type Handler' = Handler Env

type Guard' a = Guard Env a
