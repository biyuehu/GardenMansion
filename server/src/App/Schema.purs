module App.Schema where

import Models (ModelExpenses, ModelMessages, ModelMeta, ModelUsers, ReqExpenseApi, ReqExpenseDeleteApi, ReqInfoPasswordApi, ReqInfoRenameApi, ReqMessageApi, ReqMessageDeleteApi, ReqMetaApi, ReqUserApi, ReqUserDeleteApi, ReqLoginApi)
import Utils (Schema, schema)

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
