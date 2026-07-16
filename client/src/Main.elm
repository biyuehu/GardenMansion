module Main exposing (main)

import Models exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Dict
import Types exposing (Model, Msg(..), Page(..), initialModel)
import Api
import Ports
import Utils exposing (errorToString, formatTime, getUserName)

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
        Err err -> ( { model | loginError = Just (errorToString err) }, Cmd.none )
    Logout ->
      ( { initialModel | meta = model.meta, token = Nothing }
      , Cmd.batch [ Api.getMetaRequest model.token GotMeta, Ports.saveToken Nothing ]
      )
    GotMeta result ->
      case result of
        Ok meta ->
          ( { model | meta = Just meta, metaUrlInput = meta.webUrl, metaNameInput = meta.webName, metaTitleInput = meta.webTitle, metaNoticeInput = meta.webNotice }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
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
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    GotInfo result ->
      case result of
        Ok info -> ( { model | info = Just info, renameInput = info.infoNickname }, Cmd.none )
        Err err ->
          case err of
            Http.BadStatus 401 ->
              ( { model | token = Nothing, currentPage = LoginPage, errorMsg = Just "登录已过期，请重新登录" }
              , Cmd.batch [ Ports.saveToken Nothing, Api.getMetaRequest model.token GotMeta ]
              )
            _ -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    UpdateRenameInput v -> ( { model | renameInput = v }, Cmd.none )
    SubmitRename ->
      case model.token of
        Just token -> ( model, Api.putInfoRenameRequest token { infoUsername = model.renameInput } GotRenameResult )
        Nothing -> ( model, Cmd.none )
    GotRenameResult result ->
      case result of
        Ok _ -> ( { model | statusMsg = Just "昵称修改成功" }, Maybe.map (\token -> Api.getInfoRequest token GotInfo) model.token |> Maybe.withDefault Cmd.none )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
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
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    GotMessages result ->
      case result of
        Ok msgs -> ( { model | messages = List.sortBy (\m -> -m.messageReleaseTime) msgs }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
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
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    DeleteMessage id ->
      case model.token of
        Just token -> ( model, Api.deleteMessageRequest token { deleteMessageId = id } GotMessageDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotMessageDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getMessagesRequest (Maybe.withDefault "" model.token) GotMessages )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    GotExpenses result ->
      case result of
        Ok exps -> ( { model | expenses = List.sortBy (\e -> -e.expenseTime) exps }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
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
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    DeleteExpense id ->
      case model.token of
        Just token -> ( model, Api.deleteExpenseRequest token { deleteExpenseId = id } GotExpenseDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotExpenseDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getExpensesRequest (Maybe.withDefault "" model.token) GotExpenses )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    GotUsers result ->
      case result of
        Ok users ->
          let dict = List.foldl (\u acc -> Dict.insert u.userId u.userNickname acc) Dict.empty users
          in ( { model | users = users, userDict = dict }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
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
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    DeleteUser id forced ->
      case model.token of
        Just token -> ( model, Api.deleteUserRequest token { deleteUserId = id, deleteForced = forced } GotUserDeleteResult )
        Nothing -> ( model, Cmd.none )
    GotUserDeleteResult result ->
      case result of
        Ok _ -> ( model, Api.getUsersRequest (Maybe.withDefault "" model.token) GotUsers )
        Err err -> ( { model | errorMsg = Just (errorToString err) }, Cmd.none )
    DismissError -> ( { model | errorMsg = Nothing, statusMsg = Nothing }, Cmd.none )

viewHeader : Model -> Html Msg
viewHeader model =
  header [ class "text-center space-y-3" ]
    [ h1 [ class "text-4xl font-bold drop-shadow-lg text-slate-800" ] [ text ("🏠 " ++ (Maybe.map .webTitle model.meta |> Maybe.withDefault "Garden Mansion")) ]
    , case model.info of
      Just info ->
        div []
          [ p [ class "text-xl text-slate-700" ] [ text (Maybe.map .webNotice model.meta |> Maybe.withDefault "千禧年代的数字化合租生活") ]
          , div [ class "text-base flex justify-center gap-6 items-center text-slate-700 mt-2" ]
            [ span [ class "text-xl font-medium" ] [ text ("欢迎回来，" ++ info.infoNickname) ]
            , button [ class "bg-white/30 hover:bg-white/50 text-slate-700 px-3.5 py-1 rounded-lg backdrop-blur-sm transition-all cursor-pointer text-lg font-medium", onClick Logout ] [ text "登出" ]
            ]
          ]
      Nothing -> text ""
    ]

viewGlobalMessages : Model -> Html Msg
viewGlobalMessages model =
  div [ class "space-y-2" ]
    [ case model.errorMsg of
      Just err ->
        div [ class "bg-red-100 border-2 border-red-400 text-red-700 px-4 py-3 rounded-lg flex justify-between items-center" ]
          [ text ("⚠️ " ++ err), button [ onClick DismissError, class "font-bold px-2 cursor-pointer" ] [ text "×" ] ]
      Nothing -> text ""
    , case model.statusMsg of
      Just msg ->
        div [ class "bg-green-100 border-2 border-green-400 text-green-700 px-4 py-3 rounded-lg flex justify-between items-center" ]
          [ text ("✅ " ++ msg), button [ onClick DismissError, class "font-bold px-2 cursor-pointer" ] [ text "×" ] ]
      Nothing -> text ""
    ]

viewNavTab : String -> Page -> Bool -> Html Msg
viewNavTab label page isActive =
  button
    [ class (if isActive then "flex-1 px-6 py-3 rounded-lg font-medium transition-all bg-[#67A2C5] text-white shadow-md shadow-[#67A2C5]/30 cursor-pointer" else "flex-1 px-6 py-3 rounded-lg font-medium transition-all text-slate-600 hover:bg-gray/50 cursor-pointer")
    , onClick (ChangePage page)
    ]
    [ text label ]

viewNavigation : Model -> Html Msg
viewNavigation model =
  let
    isAdmin = (Maybe.map .infoLevel model.info |> Maybe.withDefault 0) > 0
  in
  nav [ class "flex gap-2 bg-white/30 p-2 rounded-xl mb-8 flex-wrap" ]
    [ viewNavTab "留言板" MessagesPage (model.currentPage == MessagesPage)
    , viewNavTab "费用管理" ExpensesPage (model.currentPage == ExpensesPage)
    , viewNavTab "室友情况" UsersPage (model.currentPage == UsersPage)
    , viewNavTab "个人信息" InfoPage (model.currentPage == InfoPage)
    , if isAdmin then viewNavTab "站点设置" MetaPage (model.currentPage == MetaPage) else text ""
    ]

viewLoginForm : Model -> Html Msg
viewLoginForm model =
  div [ class "max-w-md mx-auto space-y-6" ]
    [ h2 [ class "text-2xl font-bold text-center text-slate-800" ] [ text "🔐 登录" ]
    , case model.loginError of
      Just err -> div [ class "bg-red-100 text-red-700 px-4 py-2 rounded-lg text-sm" ] [ text err ]
      Nothing -> text ""
    , Html.form [ onSubmit SubmitLogin, class "space-y-4" ]
      [ div [ class "space-y-2" ]
        [ label [ class "block font-medium text-slate-700" ] [ text "用户名" ]
        , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "请输入用户名", value model.loginUsername, onInput UpdateLoginUsername ] []
        ]
      , div [ class "space-y-2" ]
        [ label [ class "block font-medium text-slate-700" ] [ text "密码" ]
        , input [ type_ "password", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "请输入密码", value model.loginPassword, onInput UpdateLoginPassword ] []
        ]
      , button [ type_ "button", onClick SubmitLogin, class "w-full bg-[#67A2C5] text-white px-8 py-3 rounded-lg font-medium hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all cursor-pointer" ] [ text "登录" ]
      ]
    ]

viewMessagesPage : Model -> Html Msg
viewMessagesPage model =
  div [ class "space-y-8" ]
    [ div [ class "bg-white/60 rounded-xl p-6 space-y-6" ]
      [ h3 [ class "text-xl text-slate-800" ] [ text "💬 最新留言" ]
      , if List.isEmpty model.messages then p [ class "text-slate-500 text-center py-4" ] [ text "暂无留言，来发表第一条吧" ]
        else div [ class "max-h-145 h-50vh overflow-y-auto space-y-4" ] (List.map (viewMessage model) model.messages)
      ]
    , div [ class "space-y-3" ]
      [ label [ class "block font-medium text-slate-700" ]
        [ text (case model.newMessageReplyId of
            Just id -> "回复消息 #" ++ String.fromInt id ++ "（点击已选中的按钮可取消）"
            Nothing -> "发布新留言"
          )
        ]
      , textarea [ class "w-full min-h-24 px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg text-base resize-y focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "分享你的想法或通知...", value model.newMessage, onInput UpdateNewMessage ] []
      , button [ class "bg-[#67A2C5] text-white px-8 py-3 rounded-lg font-medium hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all cursor-pointer", type_ "button", onClick SubmitMessage ] [ text "发布留言" ]
      ]
    ]

viewMessage : Model -> ResMessageSingle -> Html Msg
viewMessage model message =
  div [ class "bg-white rounded-lg p-4 border-l-4 border-[#67A2C5] shadow-sm space-y-2" ]
    [ div [ class "flex justify-between items-center" ]
      [ span [ class "font-semibold text-[#67A2C5]" ] [ text (getUserName model message.messageUserId) ]
      , div [ class "flex gap-2 items-center text-sm text-slate-500" ]
        [ span [] [ text ("#" ++ String.fromInt message.messageId) ]
        , span [] [ text (formatTime message.messageReleaseTime) ]
        ]
      ]
    , case message.messageReplyId of
      Just replyId -> p [ class "text-xs text-slate-400" ] [ text ("回复 #" ++ String.fromInt replyId) ]
      Nothing -> text ""
    , p [ class "text-slate-600 leading-relaxed" ] [ text message.messageText ]
    , div [ class "flex gap-3 text-sm" ]
      [ button [ class "bg-[#9BCEC1] hover:bg-[#67A2C5] text-white px-3 py-1 rounded-full transition-all cursor-pointer", onClick (SetReplyTo (Just message.messageId)) ] [ text "回复" ]
      , button [ class "bg-red-400 hover:bg-red-500 text-white px-3 py-1 rounded-full transition-all cursor-pointer", onClick (DeleteMessage message.messageId) ] [ text "删除" ]
      ]
    ]

viewExpensesPage : Model -> Html Msg
viewExpensesPage model =
  let total = List.foldl (\e acc -> acc + e.expenseAmount) 0 model.expenses
  in
  div [ class "space-y-8" ]
    [ div [ class "flex justify-between items-center" ]
      [ h3 [ class "text-xl text-slate-800" ] [ text "💰 本月费用统计" ]
      , div [ class "font-bold text-lg text-slate-700" ] [ text ("总计: ¥" ++ String.fromFloat total) ]
      ]
    , if List.isEmpty model.expenses then p [ class "text-slate-500 text-center py-4" ] [ text "暂无费用记录" ]
      else div [ class "max-h-145 h-50vh overflow-y-auto space-y-3" ] (List.map (viewExpense model) model.expenses)
    , div [ class "space-y-6 mt-8 bg-white/60 p-6 rounded-xl" ]
      [ div [ class "space-y-3" ]
        [ label [ class "block font-medium text-slate-700" ] [ text "金额" ]
        , input [ type_ "number", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg text-base focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "0.00", step "0.01", value model.newExpenseAmount, onInput UpdateExpenseAmount ] []
        ]
      , div [ class "space-y-3" ]
        [ label [ class "block font-medium text-slate-700" ] [ text "备注" ]
        , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg text-base focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "如：电费、水费、生活用品等", value model.newExpenseComment, onInput UpdateExpenseComment ] []
        ]
      , button [ class "bg-[#67A2C5] text-white px-8 py-3 rounded-lg font-medium hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all cursor-pointer", type_ "button", onClick SubmitExpense ] [ text "添加费用" ]
      ]
    ]

viewExpense : Model -> ResExpenseSingle -> Html Msg
viewExpense model expense =
  div [ class "flex justify-between items-center bg-white rounded-lg p-4 border border-slate-200" ]
    [ div [ class "space-y-1" ]
      [ h4 [ class "text-slate-800 font-medium" ] [ text expense.expenseComment ]
      , p [ class "text-sm text-slate-500" ] [ text (getUserName model expense.expenseUserId ++ " 代付 • " ++ formatTime expense.expenseTime) ]
      ]
    , div [ class "flex items-center gap-4" ]
      [ div [ class "font-bold text-lg text-red-500" ] [ text ("¥" ++ String.fromFloat expense.expenseAmount) ]
      , button [ class "bg-red-400 hover:bg-red-500 text-white text-sm px-3 py-1 rounded-full transition-all cursor-pointer", onClick (DeleteExpense expense.expenseId) ] [ text "删除" ]
      ]
    ]

viewUsersPage : Model -> Html Msg
viewUsersPage model =
  let
    isAdmin = (Maybe.map .infoLevel model.info |> Maybe.withDefault 0) > 0
  in
  div [ class "space-y-6" ]
    [ h3 [ class "text-xl text-slate-800" ] [ text "👥 室友列表" ]
    , if List.isEmpty model.users then p [ class "text-slate-500 text-center py-4" ] [ text "暂无室友" ]
      else div [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 justify-items-center mx-auto" ] (List.map (viewUser model) model.users)
    , if isAdmin then
        div [ class "bg-white/60 rounded-xl p-6 space-y-4 max-w-md mx-auto" ]
          [ h4 [ class "font-semibold text-slate-700" ] [ text "添加新室友" ]
          , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "登录用户名", value model.newUserName, onInput UpdateNewUserName ] []
          , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "昵称", value model.newUserNickname, onInput UpdateNewUserNickname ] []
          , input [ type_ "password", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "初始密码", value model.newUserPassword, onInput UpdateNewUserPassword ] []
          , button [ type_ "button", class "bg-[#67A2C5] text-white px-8 py-3 rounded-lg font-medium hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all w-full cursor-pointer", onClick SubmitUser ] [ text "添加室友" ]
          ]
      else text ""
    ]

viewUser : Model -> ResUserSingle -> Html Msg
viewUser model user =
  let
    isAdmin = (Maybe.map .infoLevel model.info |> Maybe.withDefault 0) > 0
    isSelf = (Maybe.map .infoId model.info |> Maybe.withDefault 0) == user.userId
    isBanned = user.userLevel < 0
    buttonText = if isBanned then "删除" else "封禁"
    forced = isBanned
  in
  div [ class "bg-white rounded-xl p-6 text-center border-2 border-[#9BCEC1] hover:border-[#67A2C5] hover:translate-y--2 hover:shadow-lg transition-all space-y-2 w-full max-w-xs mx-auto" ]
    [ div [ class "w-15 h-15 rounded-full bg-[#67A2C5] mx-auto flex items-center justify-center text-white text-2xl font-bold" ] [ text (String.left 1 user.userNickname) ]
    , div [ class "font-semibold" ] [ text user.userNickname ]
    , div [ class "text-sm text-slate-500" ] [ text ("@" ++ user.userName) ]
    , div [ class "text-xs text-slate-400" ] [ text (if user.userLevel > 0 then "管理员" else if user.userLevel < 0 then "已封禁" else "普通室友") ]
    , if isAdmin && not isSelf then
        button [ class "bg-red-400 hover:bg-red-500 text-white text-sm px-3 py-1 rounded-full transition-all cursor-pointer", onClick (DeleteUser user.userId forced) ] [ text buttonText ]
      else text ""
    ]

viewInfoPage : Model -> Html Msg
viewInfoPage model =
  case model.info of
    Nothing -> p [ class "text-slate-500 text-center py-4" ] [ text "加载中..." ]
    Just info ->
      div [ class "max-w-md mx-auto space-y-8" ]
        [ div [ class "bg-white/60 rounded-xl p-6 space-y-2" ]
          [ h3 [ class "text-xl text-slate-800" ] [ text "👤 我的信息" ]
          , p [] [ text ("用户名: " ++ info.infoName) ]
          , p [] [ text ("昵称: " ++ info.infoNickname) ]
          , p [] [ text ("权限等级: " ++ (if info.infoLevel > 0 then "管理员" else "普通用户")) ]
          ]
        , div [ class "space-y-3" ]
          [ label [ class "block font-medium text-slate-700" ] [ text "修改昵称" ]
          , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", value model.renameInput, onInput UpdateRenameInput ] []
          , button [ type_ "button", class "bg-[#67A2C5] text-white px-6 py-2 rounded-lg hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all cursor-pointer", onClick SubmitRename ] [ text "保存昵称" ]
          ]
        , div [ class "space-y-3" ]
          [ label [ class "block font-medium text-slate-700" ] [ text "修改密码" ]
          , input [ type_ "password", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "旧密码", value model.pwOldInput, onInput UpdatePwOldInput ] []
          , input [ type_ "password", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", placeholder "新密码", value model.pwNewInput, onInput UpdatePwNewInput ] []
          , button [ type_ "button", class "bg-[#67A2C5] text-white px-6 py-2 rounded-lg hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all cursor-pointer", onClick SubmitPasswordChange ] [ text "修改密码" ]
          ]
        ]

viewMetaPage : Model -> Html Msg
viewMetaPage model =
  div [ class "max-w-md mx-auto space-y-6" ]
    [ h3 [ class "text-xl text-slate-800" ] [ text "⚙️ 站点设置" ]
    , p [ class "text-sm text-slate-500" ] [ text "注意：修改站点信息需要管理员权限" ]
    , div [ class "space-y-3" ]
      [ label [ class "block font-medium text-slate-700" ] [ text "站点 URL" ]
      , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", value model.metaUrlInput, onInput UpdateMetaUrl ] []
      ]
    , div [ class "space-y-3" ]
      [ label [ class "block font-medium text-slate-700" ] [ text "站点名称" ]
      , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", value model.metaNameInput, onInput UpdateMetaName ] []
      ]
    , div [ class "space-y-3" ]
      [ label [ class "block font-medium text-slate-700" ] [ text "标题" ]
      , input [ type_ "text", class "w-full px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", value model.metaTitleInput, onInput UpdateMetaTitle ] []
      ]
    , div [ class "space-y-3" ]
      [ label [ class "block font-medium text-slate-700" ] [ text "公告" ]
      , textarea [ class "w-full min-h-20 px-3 py-2 border-solid border-2 border-[#9BCEC1] rounded-lg resize-y focus:outline-none focus:border-[#67A2C5] focus:ring-2 focus:ring-[#67A2C5] focus:ring-opacity-50 focus:shadow-lg transition-all", value model.metaNoticeInput, onInput UpdateMetaNotice ] []
      ]
    , button [ type_ "button", class "bg-[#67A2C5] text-white px-8 py-3 rounded-lg font-medium hover:bg-[#9BCEC1] shadow-md hover:shadow-lg transition-all w-full cursor-pointer", onClick SubmitMeta ] [ text "保存站点设置" ]
    ]

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
                LoginPage -> viewLoginForm model
                MessagesPage -> viewMessagesPage model
                ExpensesPage -> viewExpensesPage model
                UsersPage -> viewUsersPage model
                InfoPage -> viewInfoPage model
                MetaPage -> viewMetaPage model
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
