module Util.Debug exposing (..)


debugV : String -> v -> a -> a
debugV m v a =
    let
        _ =
            Debug.log m v
    in
        a


debugOffV : String -> v -> a -> a
debugOffV m v a =
    a


debugOn : String -> a -> a
debugOn m a =
    Debug.log m a


debugOff : String -> a -> a
debugOff m a =
    a


debug : String -> a -> a
debug m a =
    debugOff m a
