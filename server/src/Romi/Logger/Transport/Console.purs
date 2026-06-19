module Romi.Logger.Transport.Console
  ( ConsoleTransport(..)
  )
  where

import Prelude

import Data.Array (intercalate)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Romi.Logger (class Transport, LoggerLevel(..))

foreign import renderTime :: String -> Number -> String

newtype ConsoleTransport =
  ConsoleTransport
    { useColor :: Boolean
    , labelColor :: String
    , labelTemplate :: String
    , timeFormat :: String
    , template :: String
    }

instance transportConsole ::
  MonadEffect m =>
  Transport m ConsoleTransport where

  handle
    (ConsoleTransport cfg)
    data' =
      liftEffect do
        let
          color code s = if cfg.useColor then "\x1b[" <> code <> "m" <> s <> "\x1b[0m" else s

          level =
            case data'.level of
              Fatal -> color "91;1" "FATAL"
              Error -> color "31" "ERROR"
              Warn -> color "33" "WARN"
              Info -> color "32" "INFO"
              Record -> color "36" "LOG"
              Debug -> color "35" "DEBUG"
              Trace -> color "90" "TRACE"
              Silent -> ""

          labels =
            intercalate " "
              (data'.labels <#> \name ->
                replaceAll
                  (Pattern "{name}")
                  (Replacement (color cfg.labelColor name))
                  cfg.labelTemplate
              )

          msg =
            case data'.level of
              Fatal -> color "91" data'.msg
              Error -> color "31" data'.msg
              Warn -> color "93" data'.msg
              Debug -> color "35" data'.msg
              Trace -> color "90" data'.msg
              _ -> data'.msg

          time = renderTime cfg.timeFormat data'.time

          output =
            cfg.template
              # replaceAll (Pattern "{level}") (Replacement level)
              # replaceAll (Pattern "{pid}") (Replacement (show data'.pid))
              # replaceAll (Pattern "{labels}") (Replacement labels)
              # replaceAll (Pattern "{msg}") (Replacement msg)
              # replaceAll (Pattern "{time}") (Replacement time)

        case data'.level of
          Fatal ->
            Console.error output
          Error ->
            Console.error output
          _ ->
            Console.log output