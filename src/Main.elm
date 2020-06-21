module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing ()

import Browser
import Browser.Dom
import Debug
import Draggable
import Html exposing (Html, div, h3, i, text)
import Html.Attributes exposing (style)
import Svg exposing (circle, polygon, svg)
import Svg.Attributes exposing (cx, cy, fill, r)
import Svg.Events



---- MODEL ----


type alias Model =
    { ballPosition : Position
    , drag : Draggable.State String
    , isHovered : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { ballPosition = Position 300 300
      , drag = Draggable.init
      , isHovered = False
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Position =
    { x : Float
    , y : Float
    }


type Msg
    = OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | ToggleIsHovered Bool


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDragBy ( dx, dy ) ->
            let
                { x, y } =
                    model.ballPosition

                isTriangleInside =
                    Debug.log "inside" (isInside 300 0 600 520 0 520 (Basics.round model.ballPosition.x + 30) (Basics.round model.ballPosition.y + 30))
            in
            if isTriangleInside then
                ( { model | ballPosition = Position (x + dx) (y + dy) }, Cmd.none )

            else
                ( model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ToggleIsHovered isHovered ->
            ( { model | isHovered = isHovered }, Cmd.none )


triangleArea : Int -> Int -> Int -> Int -> Int -> Int -> Int
triangleArea x1 y1 x2 y2 x3 y3 =
    Basics.abs ((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) // 2)



-- ((x1*(y2-y3) + x2*(y3-y1)+ x3*(y1-y2))/2.0))


isInside : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
isInside x1 y1 x2 y2 x3 y3 x y =
    let
        a =
            triangleArea x1 y1 x2 y2 x3 y3

        a1 =
            triangleArea x y x2 y2 x3 y3

        a2 =
            triangleArea x1 y1 x y x3 y3

        a3 =
            triangleArea x1 y1 x2 y2 x y
    in
    a == a1 + a2 + a3



---- VIEW ----


viewCoordinates : Model -> Html msg
viewCoordinates model =
    div []
        [ h3 [] [ text (String.fromFloat model.ballPosition.x) ]
        , h3 [] [ text (String.fromFloat model.ballPosition.y) ]
        ]


viewTriangle : Model -> Html Msg
viewTriangle model =
    let
        width =
            "600"

        height =
            "600"

        x =
            String.fromFloat model.ballPosition.x ++ "px"

        y =
            String.fromFloat model.ballPosition.y ++ "px"

        isTriangleInside =
            Debug.log "inside" (isInside 300 0 600 520 0 520 (Basics.round model.ballPosition.x) (Basics.round model.ballPosition.y))
    in
    svg
        [ Svg.Attributes.viewBox ("0 0 " ++ width ++ " " ++ height)
        ]
        [ polygon
            [ Svg.Attributes.points "300,0 600,520 0,520"
            , Svg.Attributes.id "mainTriangle"
            , Svg.Attributes.fill "#d8d8d8"
            , Svg.Events.onMouseOut (ToggleIsHovered False)
            ]
            []
        , circle
            [ Draggable.mouseTrigger "my-element" DragMsg
            , cx x
            , cy y
            , r "20"
            , fill "#333333"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
        [ viewTriangle model
        , viewCoordinates model
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
