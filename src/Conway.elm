module Conway exposing (main)

import Browser
import Cell
import Controls
import Controls.Selection
import Grid
import Html exposing (Html)
import Seeder
import Time


type alias Model =
    { grid : Grid.Grid Cell.State
    , dimension : Grid.Dimension
    , seederSelection : Controls.Selection.State Controls.Msg ( String, Seeder.Seeder )
    }


viewState : Model -> Browser.Document Controls.Msg
viewState model =
    { title = "Conway's game of life"
    , body =
        [ Controls.gridCanvas model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeders model.seederSelection
        , Controls.gridRecycler
        , Controls.gridReseter
        , Controls.decorate
        ]
    }


initialState : ( Model, Cmd Controls.Msg )
initialState =
    let
        gridSize =
            Grid.makeDimension 10 10
    in
    ( Model
        (Grid.generate gridSize Cell.Deceased Cell.fateOf Seeder.battlefield)
        gridSize
        (Controls.Selection.State
            (Just Seeder.getDefault)
            Seeder.getCatalog
        )
    , Cmd.none
    )


subscriptions : Model -> Sub Controls.Msg
subscriptions model =
    Time.every 1000 Controls.Tick


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
                        (Tuple.second
                            (case model.seederSelection of
                                Controls.Selection.State (Just ( _, seeder )) _ ->
                                    seeder

                                Controls.Selection.State Nothing _ ->
                                    Seeder.getDefaultValue
                            )
                        )
              }
            , Cmd.none
            )

        Controls.SelectSeed seed ->
            let
                ( selectionModel, cmds ) =
                    Controls.Selection.updateSelected model.seederSelection seed
            in
            ( { model | seederSelection = selectionModel }, cmds )

        Controls.RecycleSandbox ->
            ( { model
                | grid =
                    Grid.generate
                        model.dimension
                        Cell.Deceased
                        Cell.fateOf
                        (Tuple.second
                            (case model.seederSelection of
                                Controls.Selection.State (Just ( _, seeder )) _ ->
                                    seeder

                                Controls.Selection.State Nothing _ ->
                                    Seeder.getDefaultValue
                            )
                        )
              }
            , Cmd.none
            )

        Controls.Reset ->
            initialState


main : Program () Model Controls.Msg
main =
    Browser.document
        { init = always initialState
        , update = updateState
        , subscriptions = subscriptions
        , view = viewState
        }
