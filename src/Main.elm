module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing ()

import Browser
import Browser.Dom exposing (getElement)
import Browser.Events exposing (onMouseMove)
import Color
import Debug
import Html exposing (Html, div, h2, h3, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg.Events
import Task
import TypedSvg exposing (circle, line, polygon, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, dominantBaseline, fill, points, r, stroke, strokeWidth, textAnchor, viewBox, x, x1, x2, y, y1, y2)
import TypedSvg.Types exposing (Paint(..), px)


value1 =
    "Sweet"


value2 =
    "Spicy"


value3 =
    "Sour"



---- MODEL ----


type alias Model =
    { ballPosition : Position
    , dragState : DragState
    , triangleEl : Maybe Browser.Dom.Element
    }


init : ( Model, Cmd Msg )
init =
    ( { ballPosition = Position 0.5 0.66
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



-- https://math.stackexchange.com/questions/274712/calculate-on-which-side-of-a-straight-line-is-a-given-point-located
-- we need to see if the existing point is inside or outside.  And it seems the easiest way is to check if the point
-- is to the right of the left side, and the left of the right side.


sideOfLine : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
sideOfLine ( x, y ) ( x1, y1 ) ( x2, y2 ) =
    (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)



-- check if point is inside a triangle.


isInsideTriangle : Float -> Float -> Bool
isInsideTriangle x y =
    let
        isInsideOnLeft =
            sideOfLine ( x, y ) ( 0, 1 ) ( 0.5, 0 ) < 0

        isInsideOnRight =
            sideOfLine ( x, y ) ( 1, 1 ) ( 0.5, 0 ) > 0
    in
    isInsideOnLeft && isInsideOnRight



-- UPDATE VIEW


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePosition x y ->
            let
                ( newX, newY ) =
                    case model.triangleEl of
                        Just { element } ->
                            let
                                offsetX =
                                    (x - element.x) / element.width

                                -- make sure max of y is 1 so we can't go below triangle
                                offsetY =
                                    Basics.min ((y - element.y) / element.height) 1
                            in
                            if isInsideTriangle offsetX offsetY then
                                ( offsetX, offsetY )

                            else
                                ( model.ballPosition.x, model.ballPosition.y )

                        Nothing ->
                            ( 0.5, 0.5 )
            in
            ( { model | ballPosition = Position newX newY }, Cmd.none )

        UpdateDrag dragState ->
            case dragState of
                Pressed ->
                    ( { model | dragState = dragState }, getEl )

                Released ->
                    ( { model | dragState = dragState }, Cmd.none )

        GetTriangle result ->
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
            800

        h =
            w * (Basics.sqrt 3 / 2)

        ballRadius =
            10

        circleCx =
            px (model.ballPosition.x * w)

        circleCy =
            px (model.ballPosition.y * h)

        lineColor =
            Paint (Color.rgba 0.5 0.5 0.5 1)
    in
    svg
        [ viewBox 0 0 w h
        , Html.Attributes.style "flex" "1"
        , Html.Attributes.style "padding" "4rem"
        , Html.Attributes.style "overflow" "visible"
        , Html.Attributes.style "user-select" "none"
        ]
        [ polygon
            [ points [ ( w / 2, 0 ), ( w, h ), ( 0, h ) ]
            , TypedSvg.Attributes.id "mainTriangle"
            , fill (Paint (Color.rgba 0.8 0.8 0.8 1))
            , stroke lineColor
            ]
            []
        , line [ x1 (px 0), y1 (px h), x2 circleCx, y2 circleCy, stroke lineColor, strokeWidth (px 1) ] []
        , line [ x1 (px (w / 2)), y1 (px 0), x2 circleCx, y2 circleCy, stroke lineColor, strokeWidth (px 1) ] []
        , line [ x1 (px w), y1 (px h), x2 circleCx, y2 circleCy, stroke lineColor, strokeWidth (px 1) ] []
        , text_ [ x (px -50), y (px (h + 20)) ] [ text value1 ]
        , text_ [ x (px (w / 2)), y (px -20), dominantBaseline TypedSvg.Types.DominantBaselineMiddle, textAnchor TypedSvg.Types.AnchorMiddle ] [ text value2 ]
        , text_ [ x (px (w + 10)), y (px (h + 20)) ] [ text value3 ]
        , circle
            [ cx circleCx
            , cy circleCy
            , r (px ballRadius)
            , fill (Paint (Color.rgba 0 0 0 1))
            , Svg.Events.onMouseDown (UpdateDrag Pressed)
            , Svg.Events.onMouseUp (UpdateDrag Released)
            ]
            []
        ]


viewValues : Model -> Html Msg
viewValues model =
    div [ style "flex" "1" ]
        [ h2 [] [ text "21" ]
        , h2 [] [ text "21" ]
        , h2 [] [ text "21" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ style "display" "flex" ]
            [ viewTriangle model
            , viewValues model
            ]
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
