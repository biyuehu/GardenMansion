-- Database module based on Idris2 package
module Romi.Db
  ( BatchOp(..)
  , BatchOpModel(..)
  , DB
  , DBM
  , ListModel
  , askDB
  , class ProvideDB
  , createModel
  , dbBatch
  , dbClose
  , dbCreate
  , dbDel
  , dbDelOrIf
  , dbGet
  , dbPut
  , dbPutOr
  , dbPutOrIf
  , dbmOf
  , getDB
  )
  where

import Prelude

import Control.Monad.Reader (ask)
import Control.Promise (Promise, toAffE)
import Data.Array (filter, find, length, snoc)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Romi.Common (Romi)

data DB

foreign import dbCreatePrim :: String -> String -> Effect (Promise DB)

dbCreate :: String -> String  -> Aff DB
dbCreate name prefix = toAffE $ dbCreatePrim name prefix

foreign import dbGetPrim :: DB -> String -> (String -> Maybe String) ->  Maybe String -> Effect (Promise (Maybe String))

dbGet :: DB -> String -> Aff (Maybe String)
dbGet db key = toAffE $ dbGetPrim db key Just Nothing

foreign import dbPutPrim :: DB -> String -> String -> Effect (Promise Unit)

dbPut :: DB -> String -> String -> Aff Unit
dbPut db key value = toAffE $ dbPutPrim db key value

foreign import dbPutOrPrim :: DB -> String -> String -> Effect (Promise Unit)

dbPutOr :: DB -> String -> String -> Aff Unit
dbPutOr db key value = toAffE $ dbPutOrPrim db key value

foreign import dbPutOrIfPrim :: DB -> String -> String -> (String -> Boolean) -> Effect (Promise Unit)

dbPutOrIf :: DB -> String -> String -> (String -> Boolean) -> Aff Unit
dbPutOrIf db key value cond = toAffE $ dbPutOrIfPrim db key value cond

foreign import dbDelPrim :: DB -> String -> Effect (Promise Unit)

dbDel :: DB -> String -> Aff Unit
dbDel db key = toAffE $ dbDelPrim db key

foreign import dbDelOrIfPrim :: DB -> String -> (String -> Boolean) -> Effect (Promise Unit)

dbDelOrIf :: DB -> String -> (String -> Boolean) -> Aff Unit
dbDelOrIf db key cond = toAffE $ dbDelOrIfPrim db key cond

data BatchOp = Put String String | Del String

data BatchOpModel a = PutM a String | DelM a

data BatchOpPrim

foreign import toBatchOpPutPrim :: String -> String -> BatchOpPrim

foreign import toBatchOpDelPrim :: String -> BatchOpPrim

foreign import dbBatchPrim :: DB -> Array BatchOpPrim -> Effect (Promise Unit)

dbBatch :: DB -> Array BatchOp -> Aff Unit
dbBatch db ops = toAffE $ dbBatchPrim db (map (\x ->
  case x of
    Put k v -> toBatchOpPutPrim k v
    Del k -> toBatchOpDelPrim k
  ) ops)

foreign import dbClosePrim :: DB -> Effect (Promise Unit)

dbClose :: DB -> Aff Unit
dbClose db = toAffE $ dbClosePrim db

class ProvideDB a where
  getDB :: a -> Romi a DB

instance ProvideDB DB where
  getDB = pure

askDB :: forall a. ProvideDB a => Romi a DB
askDB = ask >>= getDB


type DBM b =
  forall a . ProvideDB a => Show b =>
    { get :: b -> Romi a (Maybe String)
    , put :: b -> String -> Romi a Unit
    , putOr :: b -> String -> Romi a Unit
    , putOrIf :: b -> String -> (String -> Boolean) -> Romi a Unit
    , del :: b -> Romi a Unit
    , delOrIf :: b -> (String -> Boolean) -> Romi a Unit
    , batch :: Array (BatchOpModel b) -> Romi a Unit
    }

dbmOf :: forall a. DBM a
dbmOf =
  { get: \k -> do
      db <- askDB
      liftAff $ dbGet db $ show k
  , put: \k v -> do
      db <- askDB
      liftAff $ dbPut db (show k) v
  , putOr: \k v -> do
      db <- askDB
      liftAff $ dbPutOr db (show k) v
  , putOrIf: \k v cond -> do
      db <- askDB
      liftAff $ dbPutOrIf db (show k) v cond
  , del: \k -> do
      db <- askDB
      liftAff $ dbDel db $ show k
  , delOrIf: \k cond -> do
      db <- askDB
      liftAff $ dbDelOrIf db (show k) cond
  , batch: \ops -> do
      db <- askDB
      liftAff $ dbBatch db $ map (\x -> case x of
        PutM k v -> Put (show k) v
        DelM k -> Del (show k)
      ) ops
  }

type ListModel a b =
  { selectAll :: Romi a (Array b)
  , select :: (b -> Boolean) -> Romi a (Maybe b)
  , selectMany :: (b -> Boolean) -> Romi a (Array b)
  , insert :: b -> Romi a Unit
  , insertMany :: Array b -> Romi a Unit
  , delete :: (b -> Boolean) -> Romi a Unit
  , count :: Romi a Int
  }

type ListModelApi a b =
  { key :: a
  , decode :: String -> Array b
  , encode :: Array b -> String
  }

createModel :: forall a b c. Show a => ProvideDB c => ListModelApi a b -> ListModel c b
createModel { key, decode, encode } =
  { selectAll: do
      datas <- dbmOf.get key
      pure $ case datas of
        Just datas' -> decode datas'
        Nothing -> []
  , select: \cond -> do
      datas <- dbmOf.get key
      pure $ case datas of
        Just datas' -> find cond $ decode datas'
        Nothing -> Nothing
  , selectMany: \cond -> do
      datas <- dbmOf.get key
      pure $ case datas of
        Just datas' -> filter cond $ decode datas'
        Nothing -> []
  , insert: \v -> do
      datas <- dbmOf.get key
      dbmOf.put key $ encode case datas of
        Just datas' -> snoc (decode datas') v
        Nothing -> [v]
    , insertMany: \vs -> do
      datas <- dbmOf.get key
      dbmOf.put key $ encode case datas of
        Just datas' -> append (decode datas') vs
        Nothing -> vs
    , delete: \cond -> do
      datas <- dbmOf.get key
      dbmOf.put key $ encode case datas of
            Just datas'' -> filter (not <<< cond) $ decode datas''
            Nothing -> []
    , count: do
      datas <- dbmOf.get key
      pure $ case datas of
        Just datas' -> length $ decode datas'
        Nothing -> 0
  }
