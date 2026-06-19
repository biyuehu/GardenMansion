module Romi.Logger (Logger(..), LogData, LoggerLevel(..), createLogger, class Transport, handle, fatal, error, warn, info, record, debug, trace, label) where

import Prelude

import Data.Foldable (traverse_)
import Data.JSDate (getTime, now)
import Effect.Class (class MonadEffect, liftEffect)

foreign import pid :: Int

foreign import handleShowTypeclass :: forall a. a -> (a -> String) -> String

data LoggerLevel
  = Fatal
  | Error
  | Warn
  | Info
  | Record
  | Debug
  | Trace
  | Silent

derive instance Eq LoggerLevel
derive instance Ord LoggerLevel

type LogData =
  { level :: LoggerLevel
  , time :: Number
  , pid :: Int
  , labels :: Array String
  , msg :: String
  }

class Transport m t where
  handle :: t -> LogData -> m Unit

newtype Logger m =
  MkLogger {  fatal :: forall a. Show a => a -> m Unit
            , error :: forall a. Show a => a -> m Unit
            , warn :: forall a. Show a => a -> m Unit
            , info :: forall a. Show a => a -> m Unit
            , record :: forall a. Show a => a -> m Unit
            , debug :: forall a. Show a => a -> m Unit
            , trace :: forall a. Show a => a -> m Unit
            , label :: String -> Logger m
            }

createLogger ::  forall t m . MonadEffect m => Transport m t =>
  { level :: LoggerLevel
  , labels :: Array String
  , transports :: Array t
  }
  -> Logger m
createLogger cfg =
  let
    emit :: forall a . Show a => LoggerLevel -> a -> m Unit
    emit level msg =
      when (level <= cfg.level) do
        time <- liftEffect now
        let data' =
              { level
              , time: getTime time
              , pid
              , labels: cfg.labels
              , msg: handleShowTypeclass msg show
              }
        traverse_ (\t -> handle t data') cfg.transports
  in
    MkLogger {  fatal: emit Fatal
              , error: emit Error
              , warn: emit Warn
              , info: emit Info
              , record: emit Record
              , debug: emit Debug
              , trace: emit Trace
              , label: \name ->
                  createLogger (cfg { labels = cfg.labels <> [ name ] })
              }

fatal :: forall a m. Show a => Logger m -> a -> m Unit
fatal (MkLogger { fatal: f }) m = f m

error :: forall a m. Show a => Logger m -> a -> m Unit
error (MkLogger { error: e }) m = e m

warn :: forall a m. Show a => Logger m -> a -> m Unit
warn (MkLogger { warn: w }) m = w m

info :: forall a m. Show a => Logger m -> a -> m Unit
info (MkLogger { info: i }) m = i m

record :: forall a m. Show a => Logger m -> a -> m Unit
record (MkLogger { record: r }) m = r m

debug :: forall a m. Show a => Logger m -> a -> m Unit
debug (MkLogger { debug: d }) m = d m

trace :: forall a m. Show a => Logger m -> a -> m Unit
trace (MkLogger { trace: t }) m = t m

label :: forall m. String -> Logger m -> Logger m
label name (MkLogger { label: l }) = l name