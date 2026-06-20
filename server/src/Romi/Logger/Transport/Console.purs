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

foreign import colorize :: String -> String

foreign import getBasicData :: String -> Number -> String -> String -> { msg::String, time :: String, level:: String}

newtype ConsoleTransport =
  ConsoleTransport
    { useColor :: Boolean
    , labelTemplate :: String
    , timeFormat :: String
    , template :: String
    }

instance transportConsole :: MonadEffect m => Transport m ConsoleTransport where
  handle (ConsoleTransport cfg) data' = liftEffect do
    case data'.level of
      Fatal ->
        Console.error output
      Error ->
        Console.error output
      _ ->
        Console.log output

    where
      labels = intercalate " " (data'.labels <#> \name ->
        replaceAll (Pattern "{name}") (Replacement name) cfg.labelTemplate)

      {level, msg, time} = getBasicData cfg.timeFormat data'.time data'.msg $ show data'.level

      output =
        colorize $ cfg.template
          # replaceAll (Pattern "{level}") (Replacement level)
          # replaceAll (Pattern "{pid}") (Replacement $ show data'.pid)
          # replaceAll (Pattern "{labels}") (Replacement labels)
          # replaceAll (Pattern "{msg}") (Replacement msg)
          # replaceAll (Pattern "{time}") (Replacement time)
