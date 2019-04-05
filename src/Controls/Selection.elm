module Controls.Selection exposing
    ( Key
    , State(..)
    , render
    , renderElementFromCatalog
    , updateSelected
    )

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Key =
    Int


type State msg element
    = State (Maybe ( Key, element )) (Dict Key element)


updateSelected : State msg element -> Key -> ( State msg element, Cmd msg )
updateSelected (State maybeSelected dict) selected =
    ( State (Dict.get selected dict |> Maybe.map ((,) selected)) dict, Cmd.none )


renderElementFromCatalog :
    Dict Key ( String, element )
    -> Html msg
    -> Key
    -> Html msg
renderElementFromCatalog catalog placeholder elementKey =
    catalog
        |> Dict.filter (\currentKey ( _, _ ) -> (==) elementKey currentKey)
        |> Dict.values
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.map H.text
        |> Maybe.withDefault placeholder



{--
--}


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
                |> Dict.map
                    (renderLine renderElement elementMsg maybeSelected)
                |> Dict.values
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
