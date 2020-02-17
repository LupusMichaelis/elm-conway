module Controls.Selection exposing
    ( Key
    , State(..)
    , render
    , renderElementFromCatalog
    , updateSelected
    )

import Dict exposing (Dict)
import Dict.Nonempty
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Key =
    Int


type State msg element
    = State (Maybe ( Key, element )) (Dict.Nonempty.Nonempty Key element)


updateSelected : State msg element -> Key -> State msg element
updateSelected (State maybeSelected dict) selected =
    State (Dict.Nonempty.get selected dict |> Maybe.map (Tuple.pair selected)) dict


renderElementFromCatalog :
    Dict.Nonempty.Nonempty Int ( String, element )
    -> Html msg
    -> Key
    -> Html msg
renderElementFromCatalog catalog placeholder elementKey =
    catalog
        |> Dict.Nonempty.filter (\currentKey ( _, _ ) -> (==) elementKey currentKey)
        |> Maybe.map Dict.Nonempty.values
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.map H.text
        |> Maybe.withDefault placeholder


render :
    (Key -> Html msg)
    -> (Key -> msg)
    -> State msg element
    -> Html msg
render renderElement elementMsg (State maybeSelected elementDict) =
    let
        liList : List (Html msg)
        liList =
            elementDict
                |> Dict.Nonempty.map
                    (renderLine renderElement elementMsg maybeSelected)
                |> Dict.Nonempty.values
    in
    H.ul [] liList


renderLine :
    (Key -> Html msg)
    -> (Key -> msg)
    -> Maybe ( Key, element )
    -> Key
    -> element
    -> Html msg
renderLine renderElement elementMsg maybeSelected currentElementKey currentElementValue =
    let
        rendered : Html msg
        rendered =
            renderElement currentElementKey

        event : H.Attribute msg
        event =
            (elementMsg >> HE.onClick) currentElementKey

        attrs : List (H.Attribute msg)
        attrs =
            HA.classList
                [ ( "selected"
                  , maybeSelected
                        |> Maybe.map Tuple.first
                        |> Maybe.map ((==) currentElementKey)
                        |> Maybe.withDefault False
                  )
                ]
                :: [ event ]
    in
    H.li
        attrs
        [ rendered ]
