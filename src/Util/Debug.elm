module Util.Debug exposing (..)


debugOn : String -> a -> a
debugOn m a =
    Debug.log m a


debugOff : String -> a -> a
debugOff m a =
    a


debug : String -> a -> a
debug m a =
    debugOff m a
