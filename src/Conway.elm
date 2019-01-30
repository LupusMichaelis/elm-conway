module Conway exposing (main)

import Grid
import Grid.Cell
import Seeder

import Controls

import Debug
import Html exposing (Html)
import Array exposing (Array)

type alias Model =
    { grid: Grid.Grid
    , dimension: Grid.Dimension
    , currentSeeder: Int
    , seeders: Array (String, Int -> Grid.Cell.State)
    }

viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridRenderer model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeders model.currentSeeder model.seeders
        , Controls.gridReseter
        ]

initialState : (Model, Cmd Controls.Msg)
initialState =
    let
        gridSize = Grid.makeDimension 10 10
    in
        ( Model
            (Grid.generate gridSize Seeder.allEmpty)
            (gridSize)
            0
            (Seeder.getCatalog)
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
