{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Models where

-- Types Definition

data ReqMessageApi = ReqMessageApi
  { res_messageText :: String,
    res_messageReplyId :: Maybe Int
  }

data ResMessageSingle = ResMessageSingle
  { req_messageId :: Int,
    req_messageText :: String,
    req_messageUserId :: Int,
    req_messageReplyId :: Maybe Int,
    req_messageReleaseTime :: Double
  }

type ResMessageApi = [ResMessageSingle]

type ModelMessages = [ResMessageSingle]

data ReqExpenseApi = ReqExpenseApi
  { req_expenseAmount :: Double,
    req_expenseComment :: String
  }

data ResExpenseSingle = ResExpenseSingle
  { res_expenseId :: Int,
    res_expenseUserId :: Int,
    res_expenseAmount :: Double,
    res_expenseComment :: String,
    res_expenseTime :: Double
  }

type ResExpenseApi = [ResExpenseSingle]

type ModelExpenses = [ResExpenseSingle]

data ReqLoginApi = ReqLoginApi
  { req_loginUsername :: String,
    req_loginPassword :: String
  }

data ResLoginApi = ResLoginApi
  { token :: String
  }

data ReqInfoRenameApi = ReqInfoRenameApi
  { req_infoUsername :: String
  }

data ReqInfoPasswordApi = ReqInfoPasswordApi
  { req_infoPasswordOld :: String,
    req_infoPasswordNew :: String
  }

data ResInfoApi = ResInfoApi
  { res_infoId :: Int,
    res_infoName :: String,
    res_infoNickname :: String,
    -- res_infoEmail :: String,
    res_infoTime :: Double,
    res_infoLevel :: Int
  }

data ReqMetaApi = ReqMetaApi
  { req_webUrl :: String,
    req_webName :: String,
    req_webTitle :: String,
    req_webNotice :: String,
    req_webStartTime :: Double
  }

data ResMetaApi = ResMetaApi
  { res_webUrl :: String,
    res_webName :: String,
    res_webTitle :: String,
    res_webNotice :: String,
    res_webStartTime :: Double
  }

type ModelMeta = ResMetaApi

data ReqUserApi = ReqUserApi
  { req_userName :: String,
    req_userNickname :: String,
    req_userPassword :: String
  }

data ResUserSingle = ResUserSingle
  { res_userId :: Int,
    res_userName :: String,
    res_userNickname :: String,
    res_userTime :: Double,
    res_userLevel :: Int
  }

type ResUserApi = [ResUserSingle]

data ReqUserDeleteApi = ReqUserDeleteApi
  { req_deleteUserId :: Int,
    req_deleteForced :: Bool
  }

data ReqMessageDeleteApi = ReqMessageDeleteApi
  { req_deleteMessageId :: Int
  }

data ReqExpenseDeleteApi = ReqExpenseDeleteApi
  { req_deleteExpenseId :: Int
  }

data ModelUserSingle = ModelUserSingle
  { userId :: Int,
    userName :: String,
    userNickname :: String,
    userPassword :: String,
    -- userEmail :: String,
    userTime :: Double,
    userLevel :: Int -- -1: banned, 0: normal user, 1: admin
  }

type ModelUsers = [ModelUserSingle]
