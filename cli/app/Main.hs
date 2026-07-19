{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Database.LevelDB
  ( DB,
    Iterator,
    Options (..),
    defaultOptions,
    delete,
    get,
    iterFirst,
    iterKey,
    iterNext,
    iterValid,
    iterValue,
    open,
    put,
    withIterator,
  )
import Database.LevelDB qualified as LDB
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- -- | 四个固定的顶层 key，对应 DB 里存的四个数据表
data DBKey = Users | Expenses | Messages | Meta
  deriving (Eq, Enum, Bounded)

instance Show DBKey where
  show Users = "users"
  show Expenses = "expenses"
  show Messages = "messages"
  show Meta = "meta"

allDBKeys :: [DBKey]
allDBKeys = [minBound .. maxBound]

parseDBKey :: String -> Maybe DBKey
parseDBKey s = lookup s [(show k, k) | k <- allDBKeys]

dbKeyBS :: DBKey -> BS.ByteString
dbKeyBS = BSC.pack . show

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> runInspector dir
    _ -> do
      putStrLn "Usage: leveldb-inspector <path-to-leveldb-dir>"
      exitFailure

runInspector :: FilePath -> IO ()
runInspector dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    putStrLn $ "Error: directory does not exist: " <> dir
    exitFailure

  runResourceT $ do
    db <- open dir defaultOptions {createIfMissing = False}
    liftIO $ do
      printBanner dir
      runInputT defaultSettings (loop db)

loop :: DB -> InputT IO ()
loop db = do
  minput <- getInputLine "\ESC[1;36mleveldb>\ESC[0m "
  case minput of
    Nothing -> outputStrLn "Bye~"
    Just raw -> do
      let ws = words raw
      case ws of
        [] -> loop db
        ("exit" : _) -> outputStrLn "Bye~"
        ("quit" : _) -> outputStrLn "Bye~"
        ("help" : _) -> liftIO printHelp >> loop db
        ("keys" : _) -> liftIO printKeysList >> loop db
        ("view" : k : rest) -> liftIO (handleView db k rest) >> loop db
        ("filter" : k : rest) -> liftIO (handleFilter db k rest) >> loop db
        ("remove" : k : rest) -> liftIO (handleRemove db k rest) >> loop db
        (cmd : _) -> do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn $ "Unknown command: " <> cmd <> " (type 'help')"
          liftIO $ setSGR [Reset]
          loop db

-- ============ Banner / Help ============

printBanner :: FilePath -> IO ()
printBanner dir = do
  setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
  putStrLn "╔══════════════════════════════════════════╗"
  putStrLn "║        LevelDB Interactive Inspector      ║"
  putStrLn "╚══════════════════════════════════════════╝"
  setSGR [Reset]
  putStrLn $ "Opened: " <> dir
  putStrLn "Type 'help' for commands.\n"

printHelp :: IO ()
printHelp = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "Commands:"
  setSGR [Reset]
  putStrLn "  keys                       list the 4 valid top-level keys"
  putStrLn "  view <key>                 show all records under <key>"
  putStrLn "  filter <key> <field> <val> filter records where field == val"
  putStrLn "  remove <key> <id>          remove record with matching *Id field"
  putStrLn "  help                       show this message"
  putStrLn "  exit / quit                leave"
  putStrLn ""
  putStrLn "  <key> is one of: users | expenses | messages | meta"

printKeysList :: IO ()
printKeysList = do
  setSGR [SetColor Foreground Vivid Green]
  forM_ allDBKeys $ \k -> putStrLn ("  • " <> show k)
  setSGR [Reset]

-- ============ view ============

handleView :: DB -> String -> [String] -> IO ()
handleView db keyStr _rest =
  withParsedKey keyStr $ \dbkey -> do
    mRaw <- runResourceT $ get db LDB.defaultReadOptions (dbKeyBS dbkey)
    case mRaw of
      Nothing -> warnMissing dbkey
      Just raw -> case dbkey of
        Meta -> renderSingleJSON raw
        _ -> renderArrayJSON raw

-- ============ filter ============

handleFilter :: DB -> String -> [String] -> IO ()
handleFilter db keyStr rest =
  withParsedKey keyStr $ \dbkey ->
    case (dbkey, rest) of
      (Meta, _) -> errMsg "filter is not supported on 'meta' (single object, not an array)"
      (_, [field, val]) -> do
        mRaw <- runResourceT $ get db LDB.defaultReadOptions (dbKeyBS dbkey)
        case mRaw of
          Nothing -> warnMissing dbkey
          Just raw -> case decode (BSL.fromStrict raw) :: Maybe A.Value of
            Nothing -> errMsg "stored value is not valid JSON"
            Just (Array arr) -> do
              let matched = V.filter (matchesField field val) arr
              if V.null matched
                then infoMsg "No records matched."
                else renderJSONArrayValue matched
            Just _ -> errMsg "expected a JSON array under this key"
      (_, _) -> errMsg "usage: filter <key> <field> <value>"

matchesField :: String -> String -> A.Value -> Bool
matchesField field val (Object o) =
  case KM.lookup (AK.fromString field) o of
    Just (String t) -> T.unpack t == val
    Just (Number n) -> show n == val || (readMaybe val :: Maybe Double) == Just (realToFrac n)
    Just A.Null -> val == "null"
    Just (A.Bool b) -> show b == val
    _ -> False
matchesField _ _ _ = False

-- ============ remove ============

-- 约定：每个模型都有一个 "<name>Id" 字段作为主键（userId / expenseId / messageId）
idFieldFor :: DBKey -> Maybe String
idFieldFor Users = Just "userId"
idFieldFor Expenses = Just "expenseId"
idFieldFor Messages = Just "messageId"
idFieldFor Meta = Nothing

handleRemove :: DB -> String -> [String] -> IO ()
handleRemove db keyStr rest =
  withParsedKey keyStr $ \dbkey ->
    case (idFieldFor dbkey, rest) of
      (Nothing, _) -> errMsg "remove by id is not supported on 'meta'"
      (Just idField, [idStr]) -> do
        mRaw <- runResourceT $ get db LDB.defaultReadOptions (dbKeyBS dbkey)
        case mRaw of
          Nothing -> warnMissing dbkey
          Just raw -> case decode (BSL.fromStrict raw) :: Maybe A.Value of
            Just (Array arr) -> do
              let idKey = AK.fromString idField
                  keep v@(Object o) = case KM.lookup idKey o of
                    Just (Number n) -> show (truncate n :: Integer) /= idStr
                    _ -> True
                  keep _ = True
                  before = V.length arr
                  filtered = V.filter keep arr
                  removedCount = before - V.length filtered
              if removedCount == 0
                then infoMsg $ "No record with " <> idField <> " = " <> idStr <> " found."
                else do
                  let newRaw = BSL.toStrict (encode (Array filtered))
                  runResourceT $ put db LDB.defaultWriteOptions (dbKeyBS dbkey) newRaw
                  okMsg $ "Removed " <> show removedCount <> " record(s) from " <> show dbkey <> "."
            _ -> errMsg "expected a JSON array under this key"
      (Just _, _) -> errMsg "usage: remove <key> <id>"

-- ============ shared helpers ============

withParsedKey :: String -> (DBKey -> IO ()) -> IO ()
withParsedKey keyStr action = case parseDBKey keyStr of
  Nothing -> errMsg $ "Unknown key '" <> keyStr <> "'. Valid: users | expenses | messages | meta"
  Just k -> action k

warnMissing :: DBKey -> IO ()
warnMissing k = infoMsg $ "No data stored under key '" <> show k <> "' yet."

errMsg :: String -> IO ()
errMsg s = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn ("✗ " <> s)
  setSGR [Reset]

okMsg :: String -> IO ()
okMsg s = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn ("✓ " <> s)
  setSGR [Reset]

infoMsg :: String -> IO ()
infoMsg s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn ("• " <> s)
  setSGR [Reset]

renderSingleJSON :: BS.ByteString -> IO ()
renderSingleJSON raw = case decode (BSL.fromStrict raw) :: Maybe A.Value of
  Nothing -> errMsg "stored value is not valid JSON"
  Just v -> renderJSONValue v

renderArrayJSON :: BS.ByteString -> IO ()
renderArrayJSON raw = case decode (BSL.fromStrict raw) :: Maybe A.Value of
  Nothing -> errMsg "stored value is not valid JSON"
  Just (Array arr)
    | V.null arr -> infoMsg "Empty."
    | otherwise -> renderJSONArrayValue arr
  Just other -> renderJSONValue other

renderJSONArrayValue :: V.Vector A.Value -> IO ()
renderJSONArrayValue arr = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ "─── " <> show (V.length arr) <> " record(s) ───"
  setSGR [Reset]
  forM_ (zip [1 :: Int ..] (V.toList arr)) $ \(i, v) -> do
    setSGR [SetColor Foreground Dull White]
    putStrLn $ "[" <> show i <> "]"
    setSGR [Reset]
    renderJSONValue v
    putStrLn ""

renderJSONValue :: A.Value -> IO ()
renderJSONValue (Object o) =
  forM_ (sortOn fst (KM.toList o)) $ \(k, v) -> do
    setSGR [SetColor Foreground Vivid Magenta]
    putStr ("  " <> AK.toString k <> ": ")
    setSGR [Reset]
    TIO.putStrLn (renderScalar v)
renderJSONValue v = TIO.putStrLn (renderScalar v)

renderScalar :: A.Value -> T.Text
renderScalar (String t) = t
renderScalar (Number n) = T.pack (show n)
renderScalar (A.Bool b) = T.pack (show b)
renderScalar A.Null = "null"
renderScalar other = T.pack (show other)
