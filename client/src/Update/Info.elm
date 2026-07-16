module Update.Info exposing (update)

import Types exposing (Model, Msg(..))
import Http
import Api
import Utils
import Types exposing (Page(..))
import Ports exposing (saveToken)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotInfo result ->
      case result of
        Ok info -> ( { model | info = Just info, renameInput = info.infoNickname }, Cmd.none )
        Err err ->
          case err of
            Http.BadStatus 401 ->
                ( { model | token = Nothing, currentPage = LoginPage, errorMsg = Just "登录已过期，请重新登录" }
                , Cmd.batch [ saveToken Nothing, Api.getMetaRequest model.token GotMeta ]
                )
            _ -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdateRenameInput v -> ( { model | renameInput = v }, Cmd.none )
    SubmitRename ->
      case model.token of
        Just token -> ( model, Api.putInfoRenameRequest token { infoUsername = model.renameInput } GotRenameResult )
        Nothing -> ( model, Cmd.none )
    GotRenameResult result ->
      case result of
        Ok _ -> ( { model | statusMsg = Just "昵称修改成功" }, Maybe.map (\token -> Api.getInfoRequest token GotInfo) model.token |> Maybe.withDefault Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdatePwOldInput v -> ( { model | pwOldInput = v }, Cmd.none )
    UpdatePwNewInput v -> ( { model | pwNewInput = v }, Cmd.none )
    SubmitPasswordChange ->
      case model.token of
        Just token ->
          if String.isEmpty model.pwNewInput then ( { model | errorMsg = Just "新密码不能为空" }, Cmd.none )
          else ( model, Api.putInfoPasswordRequest token { infoPasswordOld = model.pwOldInput, infoPasswordNew = model.pwNewInput } GotPasswordResult )
        Nothing -> ( model, Cmd.none )
    GotPasswordResult result ->
      case result of
        Ok _ ->
          ( { model
              | token = Nothing
              , currentPage = LoginPage
              , statusMsg = Just "密码修改成功，请重新登录"
              , pwOldInput = ""
              , pwNewInput = ""
            }
          , Cmd.batch [ saveToken Nothing, Api.getMetaRequest model.token GotMeta ]
          )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    _ -> ( model, Cmd.none )
