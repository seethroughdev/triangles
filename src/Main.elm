module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing ()

import Bitwise exposing (and)
import Browser
import Browser.Dom exposing (getElement)
import Browser.Events exposing (onMouseMove)
import Color
import Debug
import Html exposing (Html, div, h3, i, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg.Events
import Task
import TypedSvg exposing (circle, polygon, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, points, r, viewBox)
import TypedSvg.Types exposing (Paint(..), px)



---- MODEL ----


type alias Model =
    { ballPosition : Position
    , dragState : DragState
    , triangleEl : Maybe Browser.Dom.Element
    }


init : ( Model, Cmd Msg )
init =
    ( { ballPosition = Position 0.5 0.5
      , dragState = Released
      , triangleEl = Nothing
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
    | GetTriangle (Result Browser.Dom.Error Browser.Dom.Element)


getEl : Cmd Msg
getEl =
    Task.attempt GetTriangle (getElement "mainTriangle")


sideOfLine : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
sideOfLine ( x, y ) ( x1, y1 ) ( x2, y2 ) =
    (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePosition x y ->
            let
                newX =
                    case model.triangleEl of
                        Just { element } ->
                            (x - element.x) / element.width

                        Nothing ->
                            x

                newY =
                    case model.triangleEl of
                        Just { element } ->
                            let
                                newValue =
                                    (y - element.y) / element.height
                            in
                            if newValue < 1 then
                                newValue

                            else
                                1

                        Nothing ->
                            y

                isInsideOnLeft =
                    sideOfLine ( newX, newY ) ( 0, 1 ) ( 0.5, 0 ) < 0

                isInsideOnRight =
                    sideOfLine ( newX, newY ) ( 1, 1 ) ( 0.5, 0 ) > 0

                isInside =
                    Debug.log "isInside" (isInsideOnLeft && isInsideOnRight)
            in
            ( { model | ballPosition = Position newX newY }, Cmd.none )

        UpdateDrag dragState ->
            case dragState of
                Pressed ->
                    ( { model | dragState = dragState }, getEl )

                Released ->
                    Debug.log "Released" ( { model | dragState = dragState }, Cmd.none )

        GetTriangle result ->
            let
                a =
                    Debug.log "result" result
            in
            case result of
                Ok el ->
                    ( { model | triangleEl = Just el }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- ( { model | isClicked = isClicked }, Cmd.none )
---- VIEW ----


viewCoordinates : Model -> Html msg
viewCoordinates model =
    let
        widthText =
            case model.triangleEl of
                Just el ->
                    String.fromFloat el.element.width

                Nothing ->
                    ""

        heightText =
            case model.triangleEl of
                Just el ->
                    String.fromFloat el.element.height

                Nothing ->
                    ""

        clickedText =
            if model.dragState == Pressed then
                "It is clicked"

            else
                "It is not clicked"
    in
    div []
        [ h3 [] [ text (String.fromFloat model.ballPosition.x) ]
        , h3 [] [ text (String.fromFloat model.ballPosition.y) ]
        , h3 [] [ text clickedText ]
        , h3 [] [ text ("height: " ++ widthText) ]
        , h3 [] [ text ("width: " ++ heightText) ]

        -- [ text (String.fromFloat model.triangleEl.h) ]
        ]


viewTriangle : Model -> Html Msg
viewTriangle model =
    let
        w =
            100

        h =
            w * (Basics.sqrt 3 / 2)

        ballRadius =
            2
    in
    svg
        [ viewBox 0 0 w h
        , Html.Attributes.style "border" "1px solid red"
        ]
        [ polygon
            [ points [ ( w / 2, 0 ), ( w, h ), ( 0, h ) ]
            , TypedSvg.Attributes.id "mainTriangle"
            , fill (Paint (Color.rgba 0.8 0.8 0.8 1))
            ]
            []
        , circle
            [ cx (px (model.ballPosition.x * w))
            , cy (px (model.ballPosition.y * h))
            , r (px ballRadius)
            , fill (Paint (Color.rgba 0 0 0 1))
            , Svg.Events.onMouseDown (UpdateDrag Pressed)
            , Svg.Events.onMouseUp (UpdateDrag Released)
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div [ style "position" "relative", style "margin" "14rem" ]
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
