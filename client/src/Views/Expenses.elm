module Views.Expenses exposing (view)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (Model, Msg(..))
import Utils exposing (formatTime, getUserName)

view : Model -> Html Msg
view model =
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
