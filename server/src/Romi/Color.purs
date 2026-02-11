module Romi.Color
  ( Color
  )
  where

import Prelude

import Data.Array (foldr)

data Color =
  Black String |
  Red String |
  Green String |
  Yellow String |
  Blue String |
  Magenta String |
  Cyan String |
  White String |
  BrightBlack String |
  BrightRed String |
  BrightGreen String |
  BrightYellow String |
  BrightBlue String |
  BrightMagenta String |
  BrightCyan String |
  BrightWhite String |
  BgBlack String |
  BgRed String |
  BgGreen String |
  BgYellow String |
  BgBlue String |
  BgMagenta String |
  BgCyan String |
  BgWhite String |
  BgBrightBlack String |
  BgBrightRed String |
  BgBrightGreen String |
  BgBrightYellow String |
  BgBrightBlue String |
  BgBrightMagenta String |
  BgBrightCyan String |
  BgBrightWhite String |
  Bold String |
  Italic String |
  Underline String |
  Default String |
  Compose_ (Array Color)

instance Show Color where
  show (Black s) = "\x1b[30m" <> s <> "\x1b[0m"
  show (Red s) = "\x1b[31m" <> s <> "\x1b[0m"
  show (Green s) = "\x1b[32m" <> s <> "\x1b[0m"
  show (Yellow s) = "\x1b[33m" <> s <> "\x1b[0m"
  show (Blue s) = "\x1b[34m" <> s <> "\x1b[0m"
  show (Magenta s) = "\x1b[35m" <> s <> "\x1b[0m"
  show (Cyan s) = "\x1b[36m" <> s <> "\x1b[0m"
  show (White s) = "\x1b[37m" <> s <> "\x1b[0m"
  show (BrightBlack s) = "\x1b[30;1m" <> s <> "\x1b[0m"
  show (BrightRed s) = "\x1b[31;1m" <> s <> "\x1b[0m"
  show (BrightGreen s) = "\x1b[32;1m" <> s <> "\x1b[0m"
  show (BrightYellow s) = "\x1b[33;1m" <> s <> "\x1b[0m"
  show (BrightBlue s) = "\x1b[34;1m" <> s <> "\x1b[0m"
  show (BrightMagenta s) = "\x1b[35;1m" <> s <> "\x1b[0m"
  show (BrightCyan s) = "\x1b[36;1m" <> s <> "\x1b[0m"
  show (BrightWhite s) = "\x1b[37;1m" <> s <> "\x1b[0m"
  show (BgBlack s) = "\x1b[40m" <> s <> "\x1b[0m"
  show (BgRed s) = "\x1b[41m" <> s <> "\x1b[0m"
  show (BgGreen s) = "\x1b[42m" <> s <> "\x1b[0m"
  show (BgYellow s) = "\x1b[43m" <> s <> "\x1b[0m"
  show (BgBlue s) = "\x1b[44m" <> s <> "\x1b[0m"
  show (BgMagenta s) = "\x1b[45m" <> s <> "\x1b[0m"
  show (BgCyan s) = "\x1b[46m" <> s <> "\x1b[0m"
  show (BgWhite s) = "\x1b[47m" <> s <> "\x1b[0m"
  show (BgBrightBlack s) = "\x1b[40;1m" <> s <> "\x1b[0m"
  show (BgBrightRed s) = "\x1b[41;1m" <> s <> "\x1b[0m"
  show (BgBrightGreen s) = "\x1b[42;1m" <> s <> "\x1b[0m"
  show (BgBrightYellow s) = "\x1b[43;1m" <> s <> "\x1b[0m"
  show (BgBrightBlue s) = "\x1b[44;1m" <> s <> "\x1b[0m"
  show (BgBrightMagenta s) = "\x1b[45;1m" <> s <> "\x1b[0m"
  show (BgBrightCyan s) = "\x1b[46;1m" <> s <> "\x1b[0m"
  show (BgBrightWhite s) = "\x1b[47;1m" <> s <> "\x1b[0m"
  show (Bold s) = "\x1b[1m" <> s <> "\x1b[0m"
  show (Italic s) = "\x1b[3m" <> s <> "\x1b[0m"
  show (Underline s) = "\x1b[4m" <> s <> "\x1b[0m"
  show (Default s) = "\x1b[0m" <> s <> "\x1b[0m"
  show (Compose_ cs) = foldr (<>) "" $ map show cs

instance Semigroup Color where
  append (Compose_ cs1) (Compose_ cs2) = Compose_ $ cs1 <> cs2
  append c1 (Compose_ cs) = Compose_ $ [c1] <> cs
  append (Compose_ cs) c2 = Compose_ $ cs <> [c2]
  append c1 c2 = Compose_ $ [c1, c2]

instance Monoid Color where
  mempty = Default ""
