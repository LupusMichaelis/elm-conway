module Conway exposing (main)

import Debug
import Html exposing (Html)
import List

type Msg = Nothing

type CellState =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present


type alias Model =
    { grid : List (List CellState)
    }

type alias Dimension =
    { w: Int    -- width
    , h: Int    -- height
    }
    
gridSize = Dimension 10 10

viewState : Model -> Html Msg
viewState model =
    Html.text <| "Placeholder"

initialState : (Model, Cmd Msg )
initialState =
    ( List.repeat gridSize.w Empty
        |> List.repeat gridSize.h
        |> Model
    , Cmd.none)

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

