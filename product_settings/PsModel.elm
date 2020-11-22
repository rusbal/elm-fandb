module PsModel exposing (Flags, Mode(..), Msg(..), PostResponse, Product, PsModel, Setting, Shop, build_dom_id, init, initial_model)

import Http
import Json.Decode exposing (Decoder, at, bool, decodeString, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias PostResponse =
    { success : Bool
    , path : String
    }


type Msg
    = SelectOutletSetting String
    | ProceedEdit
    | CancelEdit
    | BackToEdit
    | EnterWasPressed
    | EscWasPressed
    | Submit
    | UpdateMenuRemarks String
    | UpdateStatus String
    | UpdateDailyLimit String
    | UpdateOrderLimit String
    | TogglePublished
    | ToggleEnabled
    | ToggleEnabledForMenu
    | NoOp
    | RevertEdits Int
    | RedoEdits Int
    | HandleResponse (Result Http.Error PostResponse)


type Mode
    = TemplateSelectMode
    | TemplateEditMode
    | TemplateReviewMode


type alias Shop =
    ( Int, String )


type alias Setting =
    { id : Int
    , shop_id : Int
    , menu_remarks : String
    , status : String
    , enabled : Bool
    , enabled_for_menu : Bool
    , published : Bool
    , order_limit : Maybe Int
    , daily_limit : Maybe Int
    }


type alias Product =
    { id : Int
    , name : String
    , product_settings : List Setting
    }


type alias Flags =
    { submit_url : String
    , shops : List Shop
    , product : Product
    }


type alias PsModel =
    { flags : Flags
    , mode : Mode
    , form :
        { product_settings : List Setting
        , template : Maybe Setting
        }
    }


init : Flags -> ( PsModel, Cmd Msg )
init { submit_url, shops, product } =
    ( initial_model
        submit_url
        shops
        product
    , Cmd.batch []
    )


initial_model submit_url shops product =
    { flags =
        { submit_url = submit_url
        , shops = shops
        , product = product
        }
    , mode = TemplateSelectMode
    , form =
        { product_settings = product.product_settings
        , template = Nothing
        }
    }


build_dom_id idx name =
    name ++ "-" ++ String.fromInt idx
