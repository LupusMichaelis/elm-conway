module Controls.Selection
    exposing
        ( State
        , view
        )

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

type Msg = Select Int

type NamedElement element = Dict String element

type alias State element =
    { current : Maybe String
    , all : NamedElement element
    }


view : State element -> Html Msg
view current =
    let
        liClass : Int -> String
        liClass idx =
            if idx == current then
                "selected"

            else
                ""

        current : Maybe Int
        current =
            Array.get state.current
    in
    if Array.length current.all < 1 then
        H.div [] [ text "No selection available" ]
    else
        H.div []
            [ H.ul []
                (Dict.keys seeders
                    |> List.map viewLine
                )
            ]

viewLine : Int -> NamedElement element -> Html Msg
viewLine index (Dict name _) =
    H.li
        [ HE.onClick (Select index)
        , HA.class (liClass index)
        ]
        [ H.text name ]
