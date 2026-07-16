module Update.Login exposing (update)

import Types exposing (Model, Msg(..))
import Api
import Ports
import Utils
import Types exposing (Page(..))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateLoginUsername v -> ( { model | loginUsername = v }, Cmd.none )
    UpdateLoginPassword v -> ( { model | loginPassword = v }, Cmd.none )
    SubmitLogin ->
      if String.isEmpty (String.trim model.loginUsername) || String.isEmpty model.loginPassword then
        ( { model | loginError = Just "用户名和密码不能为空" }, Cmd.none )
      else
        ( { model | loginError = Nothing }
        , Api.loginRequest { loginUsername = model.loginUsername, loginPassword = model.loginPassword } GotLoginResult
        )
    GotLoginResult result ->
      case result of
        Ok res ->
          ( { model | token = Just res.token, currentPage = MessagesPage, loginPassword = "", loginError = Nothing }
          , Cmd.batch
            [ Api.getMessagesRequest res.token GotMessages
            , Api.getInfoRequest res.token GotInfo
            , Api.getUsersRequest res.token GotUsers
            , Ports.saveToken (Just res.token)
            ]
          )
        Err err -> ( { model | loginError = Just (Utils.errorToString err) }, Cmd.none )
    _ -> ( model, Cmd.none )
