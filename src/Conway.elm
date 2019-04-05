module Conway exposing (main)

import Controls
import Controls.Selection
import Debug
import Grid
import Grid.Cell
import Html exposing (Html)
import List.Extra
import Seeder
import Time


type alias Model =
    { grid : Grid.Grid
    , dimension : Grid.Dimension
    , seederSelection : Controls.Selection.State Controls.Msg Seeder.Seeder
    }


viewState : Model -> Html Controls.Msg
viewState model =
    Html.div []
        [ Controls.gridCanvas model.grid
        , Controls.gridDimensioner model.dimension
        , Controls.gridSeeders model.seederSelection
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
        (Controls.Selection.State
            (Seeder.getCatalog
                |> List.map Tuple.second
                |> List.head
            )
            (Seeder.getCatalog
                |> List.map Tuple.second
            )
        )
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
                        (case model.seederSelection of
                            Controls.Selection.State (Just seeder) _ ->
                                seeder

                            Controls.Selection.State Nothing _ ->
                                Seeder.allDeceased
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
                        (case model.seederSelection of
                            Controls.Selection.State (Just seeder) _ ->
                                seeder

                            Controls.Selection.State Nothing _ ->
                                Seeder.allDeceased
                        )
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
