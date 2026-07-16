module Views.Users exposing (view)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..))

view : Model -> Html Msg
view model =
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