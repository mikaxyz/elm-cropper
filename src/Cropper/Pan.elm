module Cropper.Pan exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)
import DOM


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Model =
    { position : Position
    , drag : Maybe Drag
    , boundingClientRect : DOM.Rectangle
    }


type Msg
    = Measure DOM.Rectangle
    | DragStart Position
    | DragAt Position
    | DragEnd Position


measureElement : Decoder Msg
measureElement =
    Json.Decode.map Measure (DOM.target <| DOM.boundingClientRect)


dragToPivot : Attribute Msg
dragToPivot =
    onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.map DragStart Mouse.position)
