module Update.Meta exposing (update)

import Types exposing (Model, Msg(..))
import Api
import Utils

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotMeta result ->
      case result of
        Ok meta ->
          ( { model | meta = Just meta, metaUrlInput = meta.webUrl, metaNameInput = meta.webName, metaTitleInput = meta.webTitle, metaNoticeInput = meta.webNotice }, Cmd.none )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
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
        Ok _ -> ( { model | statusMsg = Just "站点信息更新成功" }, Api.getMetaRequest  model.token GotMeta )
        Err err -> ( { model | errorMsg = Just (Utils.errorToString err) }, Cmd.none )
    _ -> ( model, Cmd.none )
