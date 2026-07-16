module Views.Messages exposing (view)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..))
import Utils exposing (formatTime, getUserName)

view : Model -> Html Msg
view model =
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
