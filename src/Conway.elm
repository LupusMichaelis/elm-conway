module Conway exposing (main)

import Basic
import Browser
import Cell
import Controls
import Controls.Canvas
import Controls.Selection
import Dimension
import Grid
import Html exposing (Html)
import List.Nonempty
import Seeder
import Settings
import Time


type alias Model =
    { settings : Settings.Type
    , grid : Grid.Grid Cell.State
    , dimension : Dimension.Two
    , seederSelection :
        Controls.Selection.State Controls.Msg ( String, Seeder.Type Cell.State )
    , shapeSelection :
        Controls.Selection.State Controls.Msg ( String, Controls.Canvas.Shape )
    , running : Bool
    , speed : Controls.Speed
    }


main : Program () Model Controls.Msg
main =
    Browser.document
        { init = always ( initialState Settings.get, Cmd.none )
        , update = \msg model -> ( updateState msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


view : Model -> Browser.Document Controls.Msg
view model =
    { title = "Conway's game of life"
    , body =
        [ Controls.gridCanvas
            (Tuple.second
                (case model.shapeSelection of
                    Controls.Selection.State (Just ( _, shape )) _ ->
                        shape

                    Controls.Selection.State Nothing _ ->
                        Controls.Canvas.getDefaultShapeValue
                )
                |> Tuple.pair model.settings.canvas
                |> Basic.uncurry
                    (\canvas shape ->
                        { canvas | shape = shape }
                    )
            )
            model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSelectSeeder model.seederSelection
        , Controls.gridSelectShape model.shapeSelection
        , Controls.gridRecycler
        , Controls.gridReseter
        , Controls.gridSlowDown model.speed
        , Controls.gridSwitch model.running
        , Controls.gridSpeedUp model.speed
        , Controls.decorate
        ]
    }


initialState : Settings.Type -> Model
initialState settings =
    Model
        settings
        (Grid.generate
            settings.gridDimension
            Cell.Deceased
            Cell.fateOf
            (Tuple.second Seeder.getDefaultValue)
        )
        settings.gridDimension
        (Controls.Selection.State
            (Just Seeder.getDefault)
            Seeder.getCatalog
        )
        (Controls.Selection.State
            (Controls.Canvas.getCatalogOfShapeEntry settings.canvas.shape)
            Controls.Canvas.getCatalogOfShape
        )
        True
        (List.Nonempty.head settings.speedList |> Tuple.first)


subscriptions : Model -> Sub Controls.Msg
subscriptions model =
    if model.running then
        Time.every
            (model.settings.speedList
                |> List.Nonempty.filter
                    (Tuple.first >> (==) model.speed)
                    (List.Nonempty.head model.settings.speedList)
                |> List.Nonempty.map Tuple.second
                |> List.Nonempty.head
                |> toFloat
            )
            Controls.Tick

    else
        Sub.none


updateState : Controls.Msg -> Model -> Model
updateState msg model =
    case msg of
        Controls.Tick now ->
            { model | grid = Grid.run model.grid }

        Controls.Resize dm n ->
            let
                current : Dimension.Two
                current =
                    model.dimension

                new : Dimension.Two
                new =
                    case dm of
                        Controls.Width ->
                            { current | w = n }

                        Controls.Height ->
                            { current | h = n }
            in
            { model
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

        Controls.SelectSeed seed ->
            let
                selectionModel =
                    Controls.Selection.updateSelected model.seederSelection seed
            in
            { model | seederSelection = selectionModel }

        Controls.SelectShape shape ->
            let
                selectionModel =
                    Controls.Selection.updateSelected model.shapeSelection shape
            in
            { model | shapeSelection = selectionModel }

        Controls.RecycleSandbox ->
            { model
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

        Controls.ToggleRunning ->
            { model | running = not model.running }

        Controls.SlowDown ->
            { model
                | speed =
                    case model.speed of
                        Controls.Slow ->
                            Controls.Slow

                        Controls.Normal ->
                            Controls.Slow

                        Controls.Fast ->
                            Controls.Normal
            }

        Controls.SpeedUp ->
            { model
                | speed =
                    case model.speed of
                        Controls.Slow ->
                            Controls.Normal

                        Controls.Normal ->
                            Controls.Fast

                        Controls.Fast ->
                            Controls.Fast
            }

        Controls.Reset ->
            initialState model.settings
