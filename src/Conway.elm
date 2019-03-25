module Conway exposing (main)

import Array exposing (Array)
import Controls
import Debug
import Grid
import Grid.Cell
import Html exposing (Html)
import Seeder
import Time

import Controls.Selection

type alias SeederSelectionState = Controls.Selection.State Seeder.Seeder

type alias Model =
    { grid : Grid.Grid
    , dimension : Grid.Dimension
    , seederSelection : SeederSelectionState
    }


viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridCanvas model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.Selection model.seederSelection
        , Controls.gridRecycler
        , Controls.gridReseter
        , Controls.decorate
        ]


initialState : ( Model, Cmd Controls.Msg )
initialState =
    let
        gridSize =
            Grid.makeDimension 10 10
    in
    ( Model
        (Grid.generate gridSize Seeder.battlefield)
        gridSize

        Seeder
        Seeder.getDefaultSeederIndex
        Seeder.getDefaultSeeder
        Seeder.getCatalog

    , Cmd.none
    )


subscriptions : Model -> Sub Controls.Msg
subscriptions model =
    Time.every Time.second Controls.Tick


updateState : Controls.Msg -> Model -> ( Model, Cmd Controls.Msg )
updateState msg model =
    case msg of
        Controls.Tick now ->
            ( { model | grid = Grid.run model.grid }, Cmd.none )

        Controls.Resize dm n ->
            let
                current : Grid.Dimension
                current =
                    model.dimension

                new : Grid.Dimension
                new =
                    case dm of
                        Controls.Width ->
                            { current | w = n }

                        Controls.Height ->
                            { current | h = n }
            in
            ( { model
                | dimension = new
                , grid =
                    Grid.makeFromGridAndResize
                        model.grid
                        new
                        model.seederSelection.current
              }
            , Cmd.none
            )

        Controls.SelectSeed idx ->
            let
                seeder : Maybe Seeder.Seeder
                seeder =
                    model.seederSelection.current
                        |> Array.get idx
                        |> Maybe.map Tuple.second
            in
            case seeder of
                Just seeder ->
                    ( { model
                        | seederSelection
                            = { current = seeder
                              , index = idx
                              }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Controls.RecycleSandbox ->
            ( { model
                | grid = Grid.generate model.dimension model.seederSelection.current
              }
            , Cmd.none
            )

        Controls.Reset ->
            initialState


main : Program Never Model Controls.Msg
main =
    Html.program
        { init = initialState
        , update = updateState
        , subscriptions = subscriptions
        , view = viewState
        }
