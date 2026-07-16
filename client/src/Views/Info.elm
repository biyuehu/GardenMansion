module Views.Info exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..))

view : Model -> Html Msg
view model =
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
