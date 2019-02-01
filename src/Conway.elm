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
    , currentSeederIndex: Int
    , currentSeeder: Seeder.Seeder
    , seeders: Array (String, Int -> Grid.Cell.State)
    }

viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridCanvas model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeders model.currentSeederIndex model.seeders
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
            (Seeder.getDefaultSeederIndex)
            (Seeder.getDefaultSeeder)
            (Seeder.getCatalog)
        , Cmd.none
        )

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
                    , grid = Grid.makeFromGridAndResize
                        model.grid
                        new
                        model.currentSeeder
                    }
                , Cmd.none
                )
        Controls.IncreaseHeight ->
            let
                current: Grid.Dimension
                current =
                    model.dimension

                new: Grid.Dimension
                new =
                    { current | h = current.h + 1}
            in
                (
                    { model
                    | dimension = new
                    , grid = Grid.makeFromGridAndResize
                        model.grid
                        new
                        model.currentSeeder
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
                    , grid = Grid.makeFromGridAndResize
                        model.grid
                        new
                        model.currentSeeder
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
                    , grid = Grid.makeFromGridAndResize
                        model.grid
                        new
                        model.currentSeeder
                    }
                , Cmd.none
                )
        Controls.SelectSeed idx ->
            let
                seeder: Maybe Seeder.Seeder
                seeder =
                    model.seeders
                        |> Array.get idx
                        |> Maybe.map Tuple.second
            in
                case seeder of
                    Just seeder ->
                        (
                            { model
                            | currentSeeder = seeder
                            , currentSeederIndex = idx
                            }
                        , Cmd.none
                        )
                    Nothing ->
                        ( model, Cmd.none )
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
