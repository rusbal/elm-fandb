port module TaTemplate exposing (main)

import Browser
import Model exposing (..)
import Subscriptions
import Update
import View


main =
    Browser.element
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }


port openWindow : String -> Cmd msg
