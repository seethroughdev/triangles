module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing ()

import Browser
import Browser.Events exposing (onMouseMove)
import Color
import Debug
import Html exposing (Html, div, h3, i, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg.Events
import TypedSvg exposing (circle, polygon, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, points, r, viewBox)
import TypedSvg.Types exposing (Paint(..), px)



---- MODEL ----


type alias Model =
    { ballPosition : Position
    , dragState : DragState
    }


init : ( Model, Cmd Msg )
init =
    ( { ballPosition = Position 300 300
      , dragState = Released
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Position =
    { x : Float
    , y : Float
    }


type DragState
    = Pressed
    | Released


type Msg
    = UpdatePosition Float Float
    | UpdateDrag DragState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePosition x y ->
            let
                newX =
                    x

                newY =
                    y
            in
            ( { model | ballPosition = Position newX newY }, Cmd.none )

        UpdateDrag dragState ->
            case dragState of
                Pressed ->
                    ( { model | dragState = dragState }, Cmd.none )

                Released ->
                    ( { model | dragState = dragState }, Cmd.none )



-- ( { model | isClicked = isClicked }, Cmd.none )
---- VIEW ----


viewCoordinates : Model -> Html msg
viewCoordinates model =
    div []
        [ h3 [] [ text (String.fromFloat model.ballPosition.x) ]
        , h3 [] [ text (String.fromFloat model.ballPosition.y) ]
        , h3 []
            [ text
                (if model.dragState == Pressed then
                    "It is clicked"

                 else
                    "It is not clicked"
                )
            ]
        ]


viewTriangle : Model -> Html Msg
viewTriangle model =
    let
        w =
            600

        h =
            520
    in
    svg
        [ viewBox 0 0 w h
        , Svg.Events.onMouseUp (UpdateDrag Released)
        ]
        [ polygon
            [ points [ ( w / 2, 0 ), ( w, h ), ( 0, h ) ]
            , TypedSvg.Attributes.id "mainTriangle"
            , fill (Paint (Color.rgba 0.8 0.8 0.8 1))
            , Svg.Events.onMouseOut (UpdateDrag Released)
            ]
            []
        , circle
            [ cx (px model.ballPosition.x)
            , cy (px model.ballPosition.y)
            , r (px 20)
            , fill (Paint (Color.rgba 0 0 0 1))
            , Svg.Events.onMouseDown (UpdateDrag Pressed)
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
    if model.dragState == Pressed then
        onMouseMove
            (Decode.map2 UpdatePosition
                (Decode.field "pageX" Decode.float)
                (Decode.field "pageY" Decode.float)
            )

    else
        Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
