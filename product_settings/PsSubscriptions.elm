module PsSubscriptions exposing (subscriptions)

import PsModel exposing (..)


subscriptions : PsModel -> Sub Msg
subscriptions model =
    -- model
    Sub.batch []
