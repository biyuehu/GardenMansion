module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model =
    { currentPage : Page
    , messages : List Message
    , expenses : List Expense
    , users : List User
    , newMessage : String
    , newExpense : ExpenseForm
    }

type Page
    = MessagesPage
    | ExpensesPage
    | UsersPage

type alias Message =
    { id : Int
    , author : String
    , content : String
    , timestamp : Int
    }

type alias Expense =
    { id : Int
    , name : String
    , amount : Float
    , paidBy : String
    , date : String
    , note : String
    }

type alias User =
    { id : Int
    , name : String
    , status : String
    , avatar : String
    }

type alias ExpenseForm =
    { name : String
    , amount : String
    , note : String
    }

initialModel : Model
initialModel =
    { currentPage = MessagesPage
    , messages = sampleMessages
    , expenses = sampleExpenses
    , users = sampleUsers
    , newMessage = ""
    , newExpense = { name = "", amount = "", note = "" }
    }

sampleMessages : List Message
sampleMessages =
    [ { id = 1, author = "小明", content = "今天买了一些生活用品，大家记得分摊费用哦~", timestamp = 1234567890 }
    , { id = 2, author = "阿花", content = "周末准备大扫除，有空的室友一起帮忙吧！", timestamp = 1234567800 }
    ]

sampleExpenses : List Expense
sampleExpenses =
    [ { id = 1, name = "电费", amount = 128.50, paidBy = "小明", date = "2024年9月", note = "" }
    , { id = 2, name = "网费", amount = 89.00, paidBy = "阿花", date = "2024年9月", note = "" }
    , { id = 3, name = "生活用品", amount = 45.80, paidBy = "小李", date = "昨天", note = "" }
    ]

sampleUsers : List User
sampleUsers =
    [ { id = 1, name = "小明", status = "在线", avatar = "明" }
    , { id = 2, name = "阿花", status = "2小时前", avatar = "花" }
    , { id = 3, name = "小李", status = "昨天", avatar = "李" }
    ]


-- UPDATE

type Msg
    = ChangePage Page
    | UpdateNewMessage String
    | AddMessage
    | UpdateExpenseName String
    | UpdateExpenseAmount String
    | UpdateExpenseNote String
    | AddExpense

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePage page ->
            { model | currentPage = page }

        UpdateNewMessage content ->
            { model | newMessage = content }

        AddMessage ->
            if String.isEmpty (String.trim model.newMessage) then
                model
            else
                let
                    newMessage =
                        { id = List.length model.messages + 1
                        , author = "我"
                        , content = model.newMessage
                        , timestamp = 1234567890
                        }
                in
                { model
                | messages = newMessage :: model.messages
                , newMessage = ""
                }

        UpdateExpenseName name ->
            let
                oldForm = model.newExpense
                newForm = { oldForm | name = name }
            in
            { model | newExpense = newForm }

        UpdateExpenseAmount amount ->
            let
                oldForm = model.newExpense
                newForm = { oldForm | amount = amount }
            in
            { model | newExpense = newForm }

        UpdateExpenseNote note ->
            let
                oldForm = model.newExpense
                newForm = { oldForm | note = note }
            in
            { model | newExpense = newForm }

        AddExpense ->
            let
                form = model.newExpense
            in
            if String.isEmpty (String.trim form.name) || String.isEmpty (String.trim form.amount) then
                model
            else
                case String.toFloat form.amount of
                    Just amountFloat ->
                        let
                            newExpense =
                                { id = List.length model.expenses + 1
                                , name = form.name
                                , amount = amountFloat
                                , paidBy = "我"
                                , date = "刚刚"
                                , note = form.note
                                }
                            resetForm = { name = "", amount = "", note = "" }
                        in
                        { model
                        | expenses = newExpense :: model.expenses
                        , newExpense = resetForm
                        }
                    Nothing ->
                        model


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "min-h-screen p-8" ]
        [ div [ class "max-w-6xl mx-auto space-y-12" ]
            [ viewHeader
            , div [ class "bg-white/95 backdrop-blur-lg rounded-3xl p-10 shadow-2xl" ]
                [ viewNavigation model.currentPage
                , viewCurrentPage model
                ]
            ]
        ]

viewHeader : Html Msg
viewHeader =
    header [ class "text-center text-white space-y-2" ]
        [ h1 [ class "text-5xl font-bold drop-shadow-lg" ]
            [ text "🏠 合租管理系统" ]
        , p [ class "text-lg opacity-90" ]
            [ text "千禧年代的数字化合租生活" ]
        ]

viewNavigation : Page -> Html Msg
viewNavigation currentPage =
    nav [ class "flex gap-2 bg-gray-50 p-2 rounded-xl mb-8" ]
        [ viewNavTab "留言板" MessagesPage (currentPage == MessagesPage)
        , viewNavTab "费用管理" ExpensesPage (currentPage == ExpensesPage)
        , viewNavTab "室友管理" UsersPage (currentPage == UsersPage)
        ]

viewNavTab : String -> Page -> Bool -> Html Msg
viewNavTab label page isActive =
    button
        [ class (if isActive then
            "flex-1 px-6 py-3 rounded-lg font-medium transition-all bg-indigo-500 text-white shadow-lg shadow-indigo-500/30"
        else
            "flex-1 px-6 py-3 rounded-lg font-medium transition-all text-slate-600 hover:bg-slate-200")
        , onClick (ChangePage page)
        ]
        [ text label ]

viewCurrentPage : Model -> Html Msg
viewCurrentPage model =
    case model.currentPage of
        MessagesPage ->
            viewMessagesPage model

        ExpensesPage ->
            viewExpensesPage model

        UsersPage ->
            viewUsersPage model

viewMessagesPage : Model -> Html Msg
viewMessagesPage model =
    div [ class "space-y-8" ]
        [ div [ class "bg-slate-50 rounded-xl p-6 space-y-6" ]
            [ h3 [ class "text-xl text-slate-800" ]
                [ text "💬 最新留言" ]
            , div [ class "space-y-4" ]
                (List.map viewMessage model.messages)
            ]
        , Html.form [ onSubmit AddMessage, class "space-y-6" ]
            [ div [ class "space-y-3" ]
                [ label [ class "block font-medium text-slate-700" ]
                    [ text "发布新留言" ]
                , textarea
                    [ class "w-full min-h-24 px-3 py-2 border-2 border-slate-200 rounded-lg text-base resize-y focus:outline-none focus:border-indigo-500 transition-colors"
                    , placeholder "分享你的想法或通知..."
                    , value model.newMessage
                    , onInput UpdateNewMessage
                    ] []
                ]
            , button
                [ class "bg-indigo-500 text-white px-8 py-3 rounded-lg font-medium hover:bg-indigo-600 hover:translate-y--1 hover:shadow-lg hover:shadow-indigo-500/30 transition-all"
                , type_ "button"
                , onClick AddMessage
                ]
                [ text "发布留言" ]
            ]
        ]

viewMessage : Message -> Html Msg
viewMessage message =
    div [ class "bg-white rounded-lg p-4 border-l-4 border-indigo-500 shadow-sm" ]
        [ div [ class "flex justify-between items-center mb-2" ]
            [ span [ class "font-semibold text-indigo-500" ]
                [ text message.author ]
            , span [ class "text-sm text-slate-500" ]
                [ text "刚刚" ]
            ]
        , p [ class "text-slate-600 leading-relaxed" ]
            [ text message.content ]
        ]

viewExpensesPage : Model -> Html Msg
viewExpensesPage model =
    div [ class "space-y-8" ]
        [ h3 [ class "text-xl text-slate-800" ]
            [ text "💰 本月费用统计" ]
        , div [ class "space-y-3" ]
            (List.map viewExpense model.expenses)
        , Html.form [ onSubmit AddExpense, class "space-y-6 mt-8" ]
            [ div [ class "space-y-3" ]
                [ label [ class "block font-medium text-slate-700" ]
                    [ text "费用名称" ]
                , input
                    [ type_ "text"
                    , class "w-full px-3 py-2 border-2 border-slate-200 rounded-lg text-base focus:outline-none focus:border-indigo-500 transition-colors"
                    , placeholder "如：电费、水费、生活用品等"
                    , value model.newExpense.name
                    , onInput UpdateExpenseName
                    ] []
                ]
            , div [ class "space-y-3" ]
                [ label [ class "block font-medium text-slate-700" ]
                    [ text "金额" ]
                , input
                    [ type_ "number"
                    , class "w-full px-3 py-2 border-2 border-slate-200 rounded-lg text-base focus:outline-none focus:border-indigo-500 transition-colors"
                    , placeholder "0.00"
                    , step "0.01"
                    , value model.newExpense.amount
                    , onInput UpdateExpenseAmount
                    ] []
                ]
            , div [ class "space-y-3" ]
                [ label [ class "block font-medium text-slate-700" ]
                    [ text "备注" ]
                , input
                    [ type_ "text"
                    , class "w-full px-3 py-2 border-2 border-slate-200 rounded-lg text-base focus:outline-none focus:border-indigo-500 transition-colors"
                    , placeholder "可选的备注信息"
                    , value model.newExpense.note
                    , onInput UpdateExpenseNote
                    ] []
                ]
            , button
                [ class "bg-indigo-500 text-white px-8 py-3 rounded-lg font-medium hover:bg-indigo-600 hover:translate-y--1 hover:shadow-lg hover:shadow-indigo-500/30 transition-all"
                , type_ "button"
                , onClick AddExpense
                ]
                [ text "添加费用" ]
            ]
        ]

viewExpense : Expense -> Html Msg
viewExpense expense =
    div [ class "flex justify-between items-center bg-white rounded-lg p-4 border border-slate-200" ]
        [ div [ class "space-y-1" ]
            [ h4 [ class "text-slate-800 font-medium" ]
                [ text expense.name ]
            , p [ class "text-sm text-slate-500" ]
                [ text (expense.paidBy ++ "代付 • " ++ expense.date) ]
            ]
        , div [ class "font-bold text-lg text-red-500" ]
            [ text ("¥" ++ String.fromFloat expense.amount) ]
        ]

viewUsersPage : Model -> Html Msg
viewUsersPage model =
    div [ class "space-y-6" ]
        [ h3 [ class "text-xl text-slate-800" ]
            [ text "👥 室友列表" ]
        , div [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4" ]
            (List.map viewUser model.users ++ [ viewAddUserCard ])
        ]

viewUser : User -> Html Msg
viewUser user =
    div [ class "bg-white rounded-xl p-6 text-center border-2 border-slate-200 hover:border-indigo-500 hover:translate-y--2 hover:shadow-lg transition-all" ]
        [ div [ class "w-15 h-15 rounded-full bg-indigo-500 mx-auto mb-4 flex items-center justify-center text-white text-2xl font-bold" ]
            [ text user.avatar ]
        , div [ class "font-semibold mb-2" ]
            [ text user.name ]
        , div [ class "text-sm text-green-500" ]
            [ text user.status ]
        ]

viewAddUserCard : Html Msg
viewAddUserCard =
    div [ class "bg-slate-50 rounded-xl p-6 text-center border-2 border-dashed border-slate-300" ]
        [ div [ class "w-15 h-15 rounded-full bg-slate-300 mx-auto mb-4 flex items-center justify-center text-slate-500 text-2xl font-bold" ]
            [ text "+" ]
        , div [ class "font-semibold text-slate-500 mb-2" ]
            [ text "添加室友" ]
        , div [ class "text-sm text-slate-500" ]
            [ text "点击邀请" ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }