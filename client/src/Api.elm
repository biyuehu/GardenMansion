module Api exposing
    ( apiBase
    , authHeaders
    , getExpensesRequest
    , getInfoRequest
    , getMessagesRequest
    , getMetaRequest
    , getUsersRequest
    , loginRequest
    , postExpenseRequest
    , postMessageRequest
    , postUserRequest
    , putInfoPasswordRequest
    , putInfoRenameRequest
    , putMetaRequest
    , deleteExpenseRequest
    , deleteMessageRequest
    , deleteUserRequest
    , resExpenseApiDecoder
    , resExpenseSingleDecoder
    , resInfoApiDecoder
    , resMessageApiDecoder
    , resMessageSingleDecoder
    , resMetaApiDecoder
    , resUserApiDecoder
    , resUserSingleDecoder
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Models exposing (..)
import Maybe exposing (withDefault)


apiBase : String
apiBase =
    "/api"

authHeaders : String -> List Http.Header
authHeaders token =
    [ Http.header "Authorization" token ]

encodeMaybeInt : Maybe Int -> Encode.Value
encodeMaybeInt maybeVal =
    case maybeVal of
        Just val -> Encode.int val
        Nothing -> Encode.null

encodeReqMessageApi : ReqMessageApi -> Encode.Value
encodeReqMessageApi body =
    Encode.object
        [ ( "messageText", Encode.string body.messageText )
        , ( "messageReplyId", encodeMaybeInt body.messageReplyId )
        ]

encodeReqExpenseApi : ReqExpenseApi -> Value
encodeReqExpenseApi body =
    Encode.object
        [ ( "expenseAmount", Encode.float body.expenseAmount )
        , ( "expenseComment", Encode.string body.expenseComment )
        ]

encodeReqLoginApi : ReqLoginApi -> Value
encodeReqLoginApi body =
    Encode.object
        [ ( "loginUsername", Encode.string body.loginUsername )
        , ( "loginPassword", Encode.string body.loginPassword )
        ]

encodeReqInfoRenameApi : ReqInfoRenameApi -> Value
encodeReqInfoRenameApi body =
    Encode.object
        [ ( "infoUsername", Encode.string body.infoUsername )
        ]

encodeReqInfoPasswordApi : ReqInfoPasswordApi -> Value
encodeReqInfoPasswordApi body =
    Encode.object
        [ ( "infoPasswordOld", Encode.string body.infoPasswordOld )
        , ( "infoPasswordNew", Encode.string body.infoPasswordNew )
        ]

encodeReqMetaApi : ReqMetaApi -> Value
encodeReqMetaApi body =
    Encode.object
        [ ( "webUrl", Encode.string body.webUrl )
        , ( "webName", Encode.string body.webName )
        , ( "webTitle", Encode.string body.webTitle )
        , ( "webNotice", Encode.string body.webNotice )
        , ( "webStartTime", Encode.float body.webStartTime )
        ]

encodeReqUserApi : ReqUserApi -> Value
encodeReqUserApi body =
    Encode.object
        [ ( "userName", Encode.string body.userName )
        , ( "userNickname", Encode.string body.userNickname )
        , ( "userPassword", Encode.string body.userPassword )
        ]

encodeReqUserDeleteApi : ReqUserDeleteApi -> Value
encodeReqUserDeleteApi body =
    Encode.object
        [ ( "deleteUserId", Encode.int body.deleteUserId )
        , ( "deleteForced", Encode.bool body.deleteForced )
        ]

encodeReqMessageDeleteApi : ReqMessageDeleteApi -> Value
encodeReqMessageDeleteApi body =
    Encode.object
        [ ( "deleteMessageId", Encode.int body.deleteMessageId )
        ]


encodeReqExpenseDeleteApi : ReqExpenseDeleteApi -> Value
encodeReqExpenseDeleteApi body =
    Encode.object
        [ ( "deleteExpenseId", Encode.int body.deleteExpenseId )
        ]
resLoginDecoder : Decoder ResLoginApi
resLoginDecoder =
    Decode.map ResLoginApi
        (Decode.field "token" Decode.string)

resMessageSingleDecoder : Decoder ResMessageSingle
resMessageSingleDecoder =
    Decode.map5 ResMessageSingle
        (Decode.field "messageId" Decode.int)
        (Decode.field "messageText" Decode.string)
        (Decode.field "messageUserId" Decode.int)
        (Decode.maybe (Decode.field "messageReplyId" Decode.int))
        (Decode.field "messageReleaseTime" Decode.float)

resMessageApiDecoder : Decoder ResMessageApi
resMessageApiDecoder =
    Decode.list resMessageSingleDecoder

resExpenseSingleDecoder : Decoder ResExpenseSingle
resExpenseSingleDecoder =
    Decode.map5 ResExpenseSingle
        (Decode.field "expenseId" Decode.int)
        (Decode.field "expenseUserId" Decode.int)
        (Decode.field "expenseAmount" Decode.float)
        (Decode.field "expenseComment" Decode.string)
        (Decode.field "expenseTime" Decode.float)

resExpenseApiDecoder : Decoder ResExpenseApi
resExpenseApiDecoder =
    Decode.list resExpenseSingleDecoder

resInfoApiDecoder : Decoder ResInfoApi
resInfoApiDecoder =
    Decode.map5 ResInfoApi
        (Decode.field "infoId" Decode.int)
        (Decode.field "infoName" Decode.string)
        (Decode.field "infoNickname" Decode.string)
        (Decode.field "infoTime" Decode.float)
        (Decode.field "infoLevel" Decode.int)

resMetaApiDecoder : Decoder ResMetaApi
resMetaApiDecoder =
    Decode.map5 ResMetaApi
        (Decode.field "webUrl" Decode.string)
        (Decode.field "webName" Decode.string)
        (Decode.field "webTitle" Decode.string)
        (Decode.field "webNotice" Decode.string)
        (Decode.field "webStartTime" Decode.float)

resUserSingleDecoder : Decoder ResUserSingle
resUserSingleDecoder =
    Decode.map5 ResUserSingle
        (Decode.field "userId" Decode.int)
        (Decode.field "userName" Decode.string)
        (Decode.field "userNickname" Decode.string)
        (Decode.field "userTime" Decode.float)
        (Decode.field "userLevel" Decode.int)

resUserApiDecoder : Decoder ResUserApi
resUserApiDecoder =
    Decode.list resUserSingleDecoder


loginRequest :
    ReqLoginApi
    -> (Result Http.Error ResLoginApi -> msg)
    -> Cmd msg
loginRequest body toMsg =
    Http.post
        { url = apiBase ++ "/login"
        , body = Http.jsonBody (encodeReqLoginApi body)
        , expect = Http.expectJson toMsg resLoginDecoder
        }

authorizedGet : String -> String -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
authorizedGet token url decoder toMsg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

getMessagesRequest : String -> (Result Http.Error ResMessageApi -> msg) -> Cmd msg
getMessagesRequest token toMsg = authorizedGet token (apiBase ++ "/messages") resMessageApiDecoder toMsg

postMessageRequest : String -> ReqMessageApi -> (Result Http.Error () -> msg) -> Cmd msg
postMessageRequest token body toMsg =
    Http.request
        { method = "POST"
        , headers = authHeaders token
        , url = apiBase ++ "/messages"
        , body = Http.jsonBody (encodeReqMessageApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

deleteMessageRequest : String -> ReqMessageDeleteApi -> (Result Http.Error () -> msg) -> Cmd msg
deleteMessageRequest token body toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeaders token
        , url = apiBase ++ "/messages"
        , body = Http.jsonBody (encodeReqMessageDeleteApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

getExpensesRequest : String -> (Result Http.Error ResExpenseApi -> msg) -> Cmd msg
getExpensesRequest token toMsg = authorizedGet token (apiBase ++ "/expenses") resExpenseApiDecoder toMsg

postExpenseRequest : String -> ReqExpenseApi -> (Result Http.Error () -> msg) -> Cmd msg
postExpenseRequest token body toMsg =
    Http.request
        { method = "POST"
        , headers = authHeaders token
        , url = apiBase ++ "/expenses"
        , body = Http.jsonBody (encodeReqExpenseApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

deleteExpenseRequest : String -> ReqExpenseDeleteApi -> (Result Http.Error () -> msg) -> Cmd msg
deleteExpenseRequest token body toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeaders token
        , url = apiBase ++ "/expenses"
        , body = Http.jsonBody (encodeReqExpenseDeleteApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

getUsersRequest : String -> (Result Http.Error ResUserApi -> msg) -> Cmd msg
getUsersRequest token toMsg = authorizedGet token (apiBase ++ "/users") resUserApiDecoder toMsg

postUserRequest : String -> ReqUserApi -> (Result Http.Error () -> msg) -> Cmd msg
postUserRequest token body toMsg =
    Http.request
        { method = "POST"
        , headers = authHeaders token
        , url = apiBase ++ "/users"
        , body = Http.jsonBody (encodeReqUserApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

deleteUserRequest : String -> ReqUserDeleteApi -> (Result Http.Error () -> msg) -> Cmd msg
deleteUserRequest token body toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeaders token
        , url = apiBase ++ "/users"
        , body = Http.jsonBody (encodeReqUserDeleteApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

getMetaRequest : Maybe String -> (Result Http.Error ResMetaApi -> msg) -> Cmd msg
getMetaRequest token toMsg = authorizedGet (withDefault "" token) (apiBase ++ "/meta") resMetaApiDecoder toMsg


putMetaRequest : String -> ReqMetaApi -> (Result Http.Error () -> msg) -> Cmd msg
putMetaRequest token body toMsg =
    Http.request
        { method = "PUT"
        , headers = authHeaders token
        , url = apiBase ++ "/meta"
        , body = Http.jsonBody (encodeReqMetaApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

getInfoRequest : String -> (Result Http.Error ResInfoApi -> msg) -> Cmd msg
getInfoRequest token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders token
        , url = apiBase ++ "/info"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg resInfoApiDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

putInfoRenameRequest : String -> ReqInfoRenameApi -> (Result Http.Error () -> msg) -> Cmd msg
putInfoRenameRequest token body toMsg =
    Http.request
        { method = "PUT"
        , headers = authHeaders token
        , url = apiBase ++ "/info/rename"
        , body = Http.jsonBody (encodeReqInfoRenameApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

putInfoPasswordRequest : String -> ReqInfoPasswordApi -> (Result Http.Error () -> msg) -> Cmd msg
putInfoPasswordRequest token body toMsg =
    Http.request
        { method = "PUT"
        , headers = authHeaders token
        , url = apiBase ++ "/info/password"
        , body = Http.jsonBody (encodeReqInfoPasswordApi body)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
