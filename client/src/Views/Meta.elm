module Views.Meta exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..))

view : Model -> Html Msg
view model =
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
