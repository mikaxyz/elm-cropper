module Cropper.Mouse exposing (..)

import Util.Debug exposing (..)
import Html.Events exposing (on)
import Html exposing (..)
import Json.Decode as Decode
import Mouse exposing (Position)


type alias Drag =
    { start : Position
    , current : Position
    , diff : Position
    }


type alias Model =
    { position : Position
    , drag : Maybe Drag
    }


initialModel : Model
initialModel =
    { position = Position 0 0
    , drag = Nothing
    }


type Msg
    = StartDrag
    | StopDrag
    | DragStart Position
    | DragAt Position
    | DragEnd Position



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag ->
            let
                _ =
                    debugOff "StartDrag" model
            in
                ( { model | drag = Just (Drag model.position model.position (Position 0 0)) }, Cmd.none )

        StopDrag ->
            let
                _ =
                    debugOff "StopDrag" model.position
            in
                ( { model | drag = Nothing }, Cmd.none )

        DragStart position ->
            let
                _ =
                    debugOff "DragStart" position
            in
                ( { model | position = position, drag = Just (Drag position position (Position 0 0)) }, Cmd.none )

        DragAt position ->
            let
                _ =
                    debugOff "DragAt" position

                drag : Drag
                drag =
                    case model.drag of
                        Just drag ->
                            let
                                diff =
                                    (Position (drag.start.x - position.x) (drag.start.y - position.y))
                            in
                                { drag | current = position, diff = diff }

                        Nothing ->
                            Drag position position (Position 0 0)
            in
                ( { model | position = position, drag = Just drag }, Cmd.none )

        DragEnd position ->
            let
                _ =
                    debugOff "DragEnd" position
            in
                ( { model | position = position, drag = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    --    let
    --        _ =
    --            debugOff "Drag" model
    --    in
    --        (Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ])
    case model.drag of
        Nothing ->
            debugOff "NoDrag" Sub.none

        Just _ ->
            debugOff "Drag" (Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ])


{-| TODO: Understand why I can not use Html.Events.onMouseDown DragStart...
-}
onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)
