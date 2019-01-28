module Conway exposing (main)

import Grid

import Debug
import Html exposing (Html)
import List

type Msg = Nothing

type alias Model =
    { grid : Grid.Grid
    }

viewState : Model -> Html Msg
viewState model =
    Html.text <| "Placeholder"

gridSize = Grid.makeGridDimension 10 10

initialState : (Model, Cmd Msg )
initialState =
    ( Grid.makeGrid gridSize Grid.Empty
        |> Model
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

updateState : Msg -> Model -> (Model, Cmd Msg)
updateState msg model =
    case msg of
        _ -> Debug.log "Implement me!" (model, Cmd.none)

main: Program Never Model Msg
main =
    Html.program
        { init = initialState
        , update = updateState
        , subscriptions = subscriptions
        , view = viewState
        }

