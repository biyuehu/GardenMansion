module Update.Users exposing (update)

import Types exposing (Model, Msg(..))
import Api
import Utils
import Dict

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotUsers result ->
      case result of
        Ok users ->
          let dict = List.foldl (\u acc -> Dict.insert u.userId u.userNickname acc) Dict.empty users
          in ( { model | users = users, userDict = dict }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdateNewUserName v -> ( { model | newUserName = v }, Cmd.none )
    UpdateNewUserNickname v -> ( { model | newUserNickname = v }, Cmd.none )
    UpdateNewUserPassword v -> ( { model | newUserPassword = v }, Cmd.none )
    SubmitUser ->
      case model.token of
        Just token ->
          if String.isEmpty (String.trim model.newUserName) || String.isEmpty model.newUserPassword then
            ( { model | errorMsg = Just "用户名和密码不能为空" }, Cmd.none )
          else
            ( model
            , Api.postUserRequest token
              { userName = model.newUserName, userNickname = model.newUserNickname, userPassword = model.newUserPassword }
              GotUserPostResult
            )
        Nothing -> ( { model | errorMsg = Just "需要管理员权限才能添加用户" }, Cmd.none )
    GotUserPostResult result ->
      case result of
        Ok _ -> ( { model | newUserName = "", newUserNickname = "", newUserPassword = "" }, Api.getUsersRequest (Maybe.withDefault "" model.token) GotUsers )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    DeleteUser id forced ->
      case model.token of
        Just token -> ( model, Api.deleteUserRequest token { deleteUserId = id, deleteForced = forced } GotUserDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotUserDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getUsersRequest (Maybe.withDefault "" model.token) GotUsers )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    _ -> ( model, Cmd.none )
