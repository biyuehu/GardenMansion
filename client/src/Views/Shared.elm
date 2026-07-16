module Views.Shared exposing (viewHeader, viewGlobalMessages, viewNavTab, viewNavigation, viewLoginForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Page(..), Msg(..))

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
