port module ProductSettings exposing (main, openWindow)

import Browser
import PsModel exposing (..)
import PsSubscriptions
import PsUpdate
import PsView


port openWindow : String -> Cmd msg


main =
    Browser.element
        { init = PsModel.init
        , view = PsView.view
        , update = PsUpdate.update
        , subscriptions = PsSubscriptions.subscriptions
        }
