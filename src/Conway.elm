module Conway exposing (main)

import Grid
import Grid.Cell
import Seeder

import Controls

import Array exposing (Array)
import Debug
import Html exposing (Html)
import Time

type alias Model =
    { grid: Grid.Grid
    , dimension: Grid.Dimension
    , currentSeeder: Int
    , seeders: Array (String, Int -> Grid.Cell.State)
    }

viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridCanvas model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeders model.currentSeeder model.seeders
        , Controls.gridReseter
        , Controls.decorate
        ]

initialState : (Model, Cmd Controls.Msg)
initialState =
    let
        gridSize = Grid.makeDimension 10 10
    in
        ( Model
            (Grid.generate gridSize Seeder.battlefield)
            (gridSize)
            5
            (Seeder.getCatalog)
        , Cmd.none
        )

getCurrentSeeder: Model -> Seeder.Seeder
getCurrentSeeder model =
    Array.get
        model.currentSeeder
        model.seeders
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Seeder.allLive -- TODO a better default

subscriptions : Model -> Sub Controls.Msg
subscriptions model =
    Time.every Time.second Controls.Tick

updateState : Controls.Msg -> Model -> (Model, Cmd Controls.Msg)
updateState msg model =
    case msg of
        Controls.Tick now ->
            ({model|grid = Grid.run model.grid}, Cmd.none)
        Controls.IncreaseWidth ->
            let
                current: Grid.Dimension
                current =
                    model.dimension

                new: Grid.Dimension
                new =
                    { current | w = current.w + 1}
            in
                (
                    { model
                    | dimension = new
                    , grid = Grid.makeFromGridAndStickColumn
                        model.grid
                        (getCurrentSeeder model)
                    }
                , Cmd.none
                )
        Controls.DecreaseWidth ->
            let
                current: Grid.Dimension
                current =
                    model.dimension

                new: Grid.Dimension
                new =
                    { current | w = current.w - 1}
            in
                (
                    { model
                    | dimension = new
                    , grid = Grid.makeFromGridAndSliceColumn model.grid new.w
                        |> Maybe.withDefault model.grid -- don't change the grid :-/
                    }
                , Cmd.none
                )
        Controls.DecreaseHeight ->
            let
                current: Grid.Dimension
                current =
                    model.dimension

                new: Grid.Dimension
                new =
                    { current | h = current.h - 1}
            in
                (
                    { model
                    | dimension = new
                    , grid = Grid.makeFromGridAndSliceRow model.grid new.h
                        |> Maybe.withDefault model.grid -- don't change the grid :-/
                    }
                , Cmd.none
                )
        Controls.ResetSandbox ->
            initialState
        _ ->
            Debug.log "Implement me!" (model, Cmd.none)

main: Program Never Model Controls.Msg
main =
    Html.program
        { init = initialState
        , update = updateState
        , subscriptions = subscriptions
        , view = viewState
        }
