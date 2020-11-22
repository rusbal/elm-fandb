module Subscriptions exposing (subscriptions)

import Model exposing (..)


subscriptions : Model -> Sub Model.Msg
subscriptions model =
  -- model
  Sub.batch []
