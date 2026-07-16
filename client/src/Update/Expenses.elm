module Update.Expenses exposing (update)

import Types exposing (Model, Msg(..))
import Api
import Utils

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
    _ -> ( model, Cmd.none )
