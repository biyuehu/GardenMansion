-- Database module based on Idris2 package
module Romi.Db
  ( BatchOp(..)
  , BatchOpModel(..)
  , DB
  , DBOps
  , ListModel
  , class ProvideDB
  , dbBatch
  , dbClose
  , dbCreate
  , dbDel
  , dbDelOrIf
  , dbGet
  , dbOpsOf
  , dbPut
  , dbPutOr
  , dbPutOrIf
  , getDB
  , makeModel
  )
  where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import Control.Promise (Promise, toAffE)
import Data.Array (filter, find, foldl, snoc)
import Data.Either (Either, fromRight)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Simple.JSON (class WriteForeign, writeJSON)

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

-- foreign import dbPutOrIfPrim :: DB -> String -> String -> (String -> Boolean) -> Effect (Promise Unit)

dbPutOrIf :: DB -> String -> String -> (String -> Boolean) -> Aff Unit
dbPutOrIf db key value cond = do
  result <- liftAff $ dbGet db key
  case result of
    Just v -> if cond v then liftAff $ dbPut db key value else pure unit
    Nothing -> liftAff $ dbPut db key value

foreign import dbDelPrim :: DB -> String -> Effect (Promise Unit)

dbDel :: DB -> String -> Aff Unit
dbDel db key = toAffE $ dbDelPrim db key

-- foreign import dbDelOrIfPrim :: DB -> String -> (String -> Boolean) -> Effect (Promise Unit)

dbDelOrIf :: DB -> String -> (String -> Boolean) -> Aff Unit
dbDelOrIf db key cond = do
  result <- liftAff $ dbGet db key
  case result of
    Just v -> if cond v then liftAff $ dbDel db key else pure unit
    Nothing -> pure unit

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

class ProvideDB env where
  getDB :: forall m. MonadReader env m => env -> m DB

type DBOps m k =
    { get :: k -> m (Maybe String)
    , put :: forall c . WriteForeign c => k -> c -> m Unit
    , putOr :: forall c . WriteForeign c => k -> c -> m Unit
    , putOrIf :: forall c . WriteForeign c => k -> c -> (String -> Boolean) -> m Unit
    , del :: k -> m Unit
    , delOrIf :: k -> (String -> Boolean) -> m Unit
    , batch :: Array (BatchOpModel k) -> m Unit
    }

dbOpsOf :: forall env keys m .
          MonadReader env m =>
          MonadAff m =>
          ProvideDB env =>
          Show keys =>
          DBOps m keys
dbOpsOf =
  { get: \k -> do
      db <- ask >>= getDB
      liftAff $ dbGet db $ show k
  , put: \k v -> do
      db <- ask >>= getDB
      liftAff $ dbPut db (show k) $ writeJSON v
  , putOr: \k v -> do
      db <- ask >>= getDB
      liftAff $ dbPutOr db (show k) $ writeJSON v
  , putOrIf: \k v cond -> do
      db <- ask >>= getDB
      liftAff $ dbPutOrIf db (show k) (writeJSON v) cond
  , del: \k -> do
      db <- ask >>= getDB
      liftAff $ dbDel db $ show k
  , delOrIf: \k cond -> do
      db <- ask >>= getDB
      liftAff $ dbDelOrIf db (show k) cond
  , batch: \ops -> do
      db <- ask >>= getDB
      liftAff $ dbBatch db $ map (\x -> case x of
        PutM k v -> Put (show k) v
        DelM k -> Del (show k)
      ) ops
  }

type ListModel m b =
  { selectAll :: m (Array b)
  , select :: (b -> Boolean) -> m (Maybe b)
  , selectMany :: (b -> Boolean) -> m (Array b)
  , update :: (b -> Boolean) -> (b -> b) -> m Unit
  , insert :: b -> m Unit
  , insertMany :: Array b -> m Unit
  , deleteAll :: (b -> Boolean) -> m Unit
  -- , count :: m Int
  , rowId :: m Int
  }

type ListModelApi keys dat =
  { key :: keys
  , parse :: String -> Either String (Array dat)
  , rowId :: dat -> Int
  }

makeModel :: forall keys dat env m.
  MonadReader env m =>
  MonadAff m =>
  Show keys =>
  WriteForeign dat =>
  ProvideDB env =>
  ListModelApi keys dat -> ListModel m dat
makeModel { key, parse, rowId } =
  let
    decode :: String -> Array dat
    decode = fromRight [] <<< parse
  in
  { selectAll: do
      datas <- dbOpsOf.get key
      pure case datas of
        Just datas' -> decode datas'
        Nothing -> []
  , select: \cond -> do
      datas <- dbOpsOf.get key
      pure $ case datas of
        Just datas' -> find cond $ decode datas'
        Nothing -> Nothing
  , selectMany: \cond -> do
      datas <- dbOpsOf.get key
      pure $ case datas of
        Just datas' -> filter cond $ decode datas'
        Nothing -> []
  ,
    update: \cond update -> do
      datas <- dbOpsOf.get key
      dbOpsOf.put key case datas of
        Just datas' -> map (\x -> if cond x then update x else x) $ decode datas'
        Nothing -> []
  , insert: \v -> do
      datas <- dbOpsOf.get key
      dbOpsOf.put key case datas of
        Just datas' -> snoc (decode datas') v
        Nothing -> [v]
      pure unit
    , insertMany: \vs -> do
      datas <- dbOpsOf.get key
      dbOpsOf.put key case datas of
        Just datas' -> append (decode datas') vs
        Nothing -> vs
    , deleteAll: \cond -> do
      datas <- dbOpsOf.get key
      dbOpsOf.put key case datas of
            Just datas'' -> filter (not <<< cond) $ decode datas''
            Nothing -> []
    -- , count: do
    --   datas <- dbOpsOf.get key
    --   pure $ case datas of
    --     Just datas' -> length $ decode datas'
    --     Nothing -> 0
    , rowId: do
      datas <- dbOpsOf.get key
      pure $ 1 + case datas of
        Just datas' -> foldl (\acc item -> max acc $ rowId item) 0 $ decode datas'
        Nothing -> 0
  }
