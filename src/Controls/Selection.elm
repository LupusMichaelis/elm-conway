module Controls.Selection exposing
    ( State(..)
    , render
    , renderElementFromCatalog
    , updateSelected
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type State msg element
    = State (Maybe element) (List element)


updateSelected : State msg element -> element -> ( State msg element, Cmd msg )
updateSelected (State maybeSelected list) selected =
    ( State (Just selected) list, Cmd.none )


renderElementFromCatalog :
    List ( String, element )
    -> Html msg
    -> element
    -> Html msg
renderElementFromCatalog catalog placeholder element =
    catalog
        |> List.filter (Tuple.second >> (==) element)
        |> List.map Tuple.first
        |> List.head
        |> Maybe.map H.text
        |> Maybe.withDefault placeholder


render :
    (element -> Html msg)
    -> (element -> msg)
    -> State msg element
    -> Html msg
render renderElement elementMsg (State maybeSelected elementList) =
    let
        liList : List (Html msg)
        liList =
            elementList
                |> List.map
                    (renderLine maybeSelected elementMsg renderElement)
    in
    H.ul [] liList


renderLine :
    Maybe element
    -> (element -> msg)
    -> (element -> Html msg)
    -> element
    -> Html msg
renderLine maybeSelected elementMsg renderElement current =
    let
        rendered : Html msg
        rendered =
            renderElement current

        event : Maybe (H.Attribute msg)
        event =
            maybeSelected
                |> Maybe.map (elementMsg >> HE.onClick)

        attrs : List (H.Attribute msg)
        attrs =
            HA.classList
                [ ( "selected"
                  , maybeSelected
                        |> Maybe.map ((==) current)
                        |> Maybe.withDefault False
                  )
                ]
                :: List.filterMap identity [ event ]
    in
    H.li
        attrs
        [ rendered ]
