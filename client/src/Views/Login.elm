module Views.Login exposing (view)

import Html exposing (..)
import Types exposing (Model, Msg)
import Views.Shared exposing (viewLoginForm)

view : Model -> Html Msg
view model = viewLoginForm model
