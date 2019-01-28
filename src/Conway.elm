module Conway exposing (main)

import Grid

import Debug
import Html exposing (Html)
import List

type Msg = Nothing

type alias Model =
    { grid : Grid.Grid
    , dimension : Grid.Dimension
    }

viewState : Model -> Html Msg
viewState model =
    Html.text <| "Placeholder"

initialState : (Model, Cmd Msg )
initialState =
    let
        gridSize = Grid.makeDimension 10 10
    in
        ( Model
            (Grid.make gridSize Grid.Empty)
            (gridSize)
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

