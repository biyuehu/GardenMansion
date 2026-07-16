module Main exposing (main)

import Browser
import Html exposing (..)
import Types exposing (Model, Msg(..), initialModel)
import Api
import Ports
import Utils
import Views.Shared exposing (viewHeader, viewGlobalMessages, viewNavigation, viewLoginForm)
import Views.Login as LoginView
import Views.Messages as MessagesView
import Views.Expenses as ExpensesView
import Views.Users as UsersView
import Views.Info as InfoView
import Views.Meta as MetaView
import Dict
import Types exposing (Page(..))
import Html.Attributes exposing (class)
import Http

init : Maybe String -> ( Model, Cmd Msg )
init flagsToken =
  let
    model =
      { initialModel
        | token = flagsToken
        , currentPage =
            case flagsToken of
              Just _ -> MessagesPage
              Nothing -> LoginPage
      }
    cmds =
      case flagsToken of
        Just token ->
          Cmd.batch
            [ Api.getMetaRequest model.token GotMeta
            , Api.getMessagesRequest token GotMessages
            , Api.getInfoRequest token GotInfo
            , Api.getUsersRequest token GotUsers
            ]
        Nothing ->
          Api.getMetaRequest model.token GotMeta
  in
  ( model, cmds )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangePage page ->
      let cmd = case ( page, model.token ) of
            ( MessagesPage, Just token ) -> Api.getMessagesRequest token GotMessages
            ( ExpensesPage, Just token ) -> Api.getExpensesRequest token GotExpenses
            ( UsersPage, Just token ) -> Api.getUsersRequest token GotUsers
            ( InfoPage, Just token ) -> Api.getInfoRequest token GotInfo
            ( MetaPage, _ ) -> Api.getMetaRequest model.token GotMeta
            _ -> Cmd.none
      in ( { model | currentPage = page, errorMsg = Nothing, statusMsg = Nothing }, cmd )
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
    Logout ->
      ( { initialModel | meta = model.meta, token = Nothing }
      , Cmd.batch [ Api.getMetaRequest model.token GotMeta, Ports.saveToken Nothing ]
      )
    GotMeta result ->
      case result of
        Ok meta ->
          ( { model | meta = Just meta, metaUrlInput = meta.webUrl, metaNameInput = meta.webName, metaTitleInput = meta.webTitle, metaNoticeInput = meta.webNotice }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdateMetaUrl v -> ( { model | metaUrlInput = v }, Cmd.none )
    UpdateMetaName v -> ( { model | metaNameInput = v }, Cmd.none )
    UpdateMetaTitle v -> ( { model | metaTitleInput = v }, Cmd.none )
    UpdateMetaNotice v -> ( { model | metaNoticeInput = v }, Cmd.none )
    SubmitMeta ->
      case model.token of
        Just token ->
          ( model
          , Api.putMetaRequest token
            { webUrl = model.metaUrlInput, webName = model.metaNameInput, webTitle = model.metaTitleInput, webNotice = model.metaNoticeInput
            , webStartTime = Maybe.map .webStartTime model.meta |> Maybe.withDefault 0
            }
            GotMetaUpdateResult
          )
        Nothing -> ( { model | errorMsg = Just "请先登录后再修改站点信息" }, Cmd.none )
    GotMetaUpdateResult result ->
      case result of
        Ok _ -> ( { model | statusMsg = Just "站点信息更新成功" }, Api.getMetaRequest model.token GotMeta )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    GotInfo result ->
      case result of
        Ok info -> ( { model | info = Just info, renameInput = info.infoNickname }, Cmd.none )
        Err err ->
          case err of
            Http.BadStatus 401 ->
              ( { model | token = Nothing, currentPage = LoginPage, errorMsg = Just "登录已过期，请重新登录" }
              , Cmd.batch [ Ports.saveToken Nothing, Api.getMetaRequest model.token GotMeta ]
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
            , statusMsg = Just "密码修改成功，请使用新密码重新登录"
            , pwOldInput = ""
            , pwNewInput = ""
            }
          , Cmd.batch [ Ports.saveToken Nothing, Api.getMetaRequest model.token GotMeta ]
          )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    GotMessages result ->
      case result of
        Ok msgs -> ( { model | messages = List.sortBy (\m -> -m.messageReleaseTime) msgs }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdateNewMessage v -> ( { model | newMessage = v }, Cmd.none )
    SetReplyTo maybeId -> ( { model | newMessageReplyId = maybeId }, Cmd.none )
    SubmitMessage ->
      case model.token of
        Just token ->
          if String.isEmpty (String.trim model.newMessage) then ( model, Cmd.none )
          else ( model, Api.postMessageRequest token { messageText = model.newMessage, messageReplyId = model.newMessageReplyId } GotMessagePostResult )
        Nothing -> ( { model | errorMsg = Just "请先登录后再留言" }, Cmd.none )
    GotMessagePostResult result ->
      case result of
        Ok _ -> ( { model | newMessage = "", newMessageReplyId = Nothing }, Api.getMessagesRequest (Maybe.withDefault "" model.token) GotMessages )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    DeleteMessage id ->
      case model.token of
        Just token -> ( model, Api.deleteMessageRequest token { deleteMessageId = id } GotMessageDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotMessageDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getMessagesRequest (Maybe.withDefault "" model.token) GotMessages )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    GotExpenses result ->
      case result of
        Ok exps -> ( { model | expenses = List.sortBy (\e -> -e.expenseTime) exps }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    UpdateExpenseAmount v -> ( { model | newExpenseAmount = v }, Cmd.none )
    UpdateExpenseComment v -> ( { model | newExpenseComment = v }, Cmd.none )
    SubmitExpense ->
      case model.token of
        Just token ->
          case String.toFloat model.newExpenseAmount of
            Just amount -> ( model, Api.postExpenseRequest token { expenseAmount = amount, expenseComment = model.newExpenseComment } GotExpensePostResult )
            Nothing -> ( { model | errorMsg = Just "金额必须为数字" }, Cmd.none )
        Nothing -> ( { model | errorMsg = Just "请先登录后再记账" }, Cmd.none )
    GotExpensePostResult result ->
      case result of
        Ok _ -> ( { model | newExpenseAmount = "", newExpenseComment = "" }, Api.getExpensesRequest (Maybe.withDefault "" model.token) GotExpenses )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    DeleteExpense id ->
      case model.token of
        Just token -> ( model, Api.deleteExpenseRequest token { deleteExpenseId = id } GotExpenseDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotExpenseDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getExpensesRequest (Maybe.withDefault "" model.token) GotExpenses )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
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
    DismissError -> ( { model | errorMsg = Nothing, statusMsg = Nothing }, Cmd.none )

view : Model -> Html Msg
view model =
  div [ class "min-h-screen p-8 bg-gradient-to-br from-[#FFB6A6] to-[#FFEBD3]" ]
    [ div [ class "max-w-6xl mx-auto space-y-8" ]
      [ viewHeader model
      , viewGlobalMessages model
      , div [ class "bg-white/90 backdrop-blur-lg rounded-3xl p-10 shadow-2xl" ]
        (case model.token of
          Nothing -> [ viewLoginForm model ]
          Just _ ->
            [ viewNavigation model
            , case model.currentPage of
                LoginPage -> LoginView.view model
                MessagesPage -> MessagesView.view model
                ExpensesPage -> ExpensesView.view model
                UsersPage -> UsersView.view model
                InfoPage -> InfoView.view model
                MetaPage -> MetaView.view model
            ]
        )
      ]
    ]

main : Program (Maybe String) Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
