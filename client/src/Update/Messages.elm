module Update.Messages exposing (update)

import Types exposing (Model, Msg(..))
import Api
import Utils

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
    _ -> ( model, Cmd.none )
