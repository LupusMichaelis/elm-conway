module Conway exposing (main)

import Grid
import Grid.Cell

import Controls

import Debug
import Html exposing (Html)
import List

type alias Model =
    { grid : Grid.Grid
    , dimension : Grid.Dimension
    }

viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridRenderer model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeder
        , Controls.gridReseter
        ]

initialState : (Model, Cmd Controls.Msg)
initialState =
    let
        gridSize = Grid.makeDimension 10 10
    in
        ( Model
            (Grid.generate gridSize (\_ -> Grid.Cell.Empty))
            (gridSize)
        , Cmd.none
        )

subscriptions : Model -> Sub Controls.Msg
subscriptions model =
    Sub.none

updateState : Controls.Msg -> Model -> (Model, Cmd Controls.Msg)
updateState msg model =
    case msg of
        _ -> Debug.log "Implement me!" (model, Cmd.none)

main: Program Never Model Controls.Msg
main =
    Html.program
        { init = initialState
        , update = updateState
        , subscriptions = subscriptions
        , view = viewState
        }
