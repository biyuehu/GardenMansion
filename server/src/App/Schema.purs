module App.Schema where

import Prelude

import App.Types (ApplicationConfig)
import Models (ModelExpenses, ModelMessages, ModelMeta, ModelUsers, ReqExpenseApi, ReqExpenseDeleteApi, ReqInfoPasswordApi, ReqInfoRenameApi, ReqMessageApi, ReqMessageDeleteApi, ReqMetaApi, ReqUserApi, ReqUserDeleteApi, ReqLoginApi)
import Data.Either (Either(..))
import Data.List (List(..), foldr)
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (NonEmpty(..))
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, readJSON)

showErrors :: List ForeignError -> String
showErrors = foldr (\err acc -> acc <> showError err <> ";") ""
  where
    showError :: ForeignError -> String
    showError (ForeignError err) = err
    showError (TypeMismatch expected actual) = "Expected " <> expected <> " but got " <> actual
    showError (ErrorAtIndex i err) = "Error at index " <> show i <> ": " <> showError err
    showError (ErrorAtProperty prop err) = "Error at property " <> prop <> ": " <> showError err


type Schema a = String -> Either String a

schema :: forall a. ReadForeign a => Schema a
schema str = case readJSON str of
    Left (NonEmptyList (NonEmpty err a)) -> Left $ showErrors $ Cons err a
    Right x -> Right x

parseReqMessageApi :: Schema ReqMessageApi
parseReqMessageApi = schema

parseModelMessages :: Schema ModelMessages
parseModelMessages = schema

parseReqExpenseApi :: Schema ReqExpenseApi
parseReqExpenseApi = schema

parseModelExpenses :: Schema ModelExpenses
parseModelExpenses = schema

parseReqInfoRenameApi :: Schema ReqInfoRenameApi
parseReqInfoRenameApi = schema

parseReqInfoPasswordApi :: Schema ReqInfoPasswordApi
parseReqInfoPasswordApi = schema

parseReqMetaApi :: Schema ReqMetaApi
parseReqMetaApi = schema

parseModelMeta :: Schema ModelMeta
parseModelMeta = schema

parseReqUserApi :: Schema ReqUserApi
parseReqUserApi = schema

parseReqUserDeleteApi :: Schema ReqUserDeleteApi
parseReqUserDeleteApi = schema

parseReqMessageDeleteApi :: Schema ReqMessageDeleteApi
parseReqMessageDeleteApi = schema

parseReqExpenseDeleteApi :: Schema ReqExpenseDeleteApi
parseReqExpenseDeleteApi = schema

parseModelUsers :: Schema ModelUsers
parseModelUsers = schema

parseLoginInApi :: Schema ReqLoginApi
parseLoginInApi = schema

parseConfig :: Schema ApplicationConfig
parseConfig = schema
