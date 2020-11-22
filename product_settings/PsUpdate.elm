port module PsUpdate exposing (update)

import Browser.Dom as Dom
import Http
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List.Extra exposing (..)
import PsModel exposing (..)
import Rails
import Task


port openWindow : String -> Cmd msg


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> PsModel -> ( PsModel, Cmd Msg )
update msg model =
    case msg of
        SelectOutletSetting strIndex ->
            model
                |> set_template strIndex
                |> set_command msg

        ProceedEdit ->
            model
                |> apply_template
                |> set_command msg

        EnterWasPressed ->
            model
                |> apply_template
                |> set_command msg

        CancelEdit ->
            model
                |> unset_template
                |> reset_product_settings
                |> set_command msg

        EscWasPressed ->
            model
                |> unset_template
                |> reset_product_settings
                |> set_command msg

        BackToEdit ->
            model
                |> back_to_edit_template
                |> set_command msg

        RevertEdits idx ->
            model
                |> revert_edits idx
                |> set_command msg

        RedoEdits idx ->
            model
                |> redo_edits idx
                |> set_command msg

        Submit ->
            submit model

        HandleResponse result ->
            case result of
                Ok hash ->
                    ( model, openWindow hash.path )

                _ ->
                    ( model, Cmd.none )

        _ ->
            model
                |> update_model msg
                |> set_command msg


submit model =
    let
        body =
            request model
                |> Encode.object
                |> Http.jsonBody

        expect =
            Rails.expectJson HandleResponse decoder
    in
    ( model
    , Rails.post
        { url = model.flags.submit_url
        , body = body
        , expect = expect
        }
    )


request model =
    let
        product_settings =
            List.map encode_product_setting model.form.product_settings
    in
    [ ( "id", Encode.int model.flags.product.id )
    , ( "product_settings", Encode.list Encode.object product_settings )
    ]


encode_product_setting setting =
    let
        order_limit =
            case setting.order_limit of
                Just val ->
                    String.fromInt val

                _ ->
                    ""

        daily_limit =
            case setting.daily_limit of
                Just val ->
                    String.fromInt val

                _ ->
                    ""
    in
    [ ( "id", Encode.int setting.id )
    , ( "shop_id", Encode.int setting.shop_id )
    , ( "menu_remarks", Encode.string setting.menu_remarks )
    , ( "status", Encode.string setting.status )
    , ( "enabled", Encode.bool setting.enabled )
    , ( "enabled_for_menu", Encode.bool setting.enabled_for_menu )
    , ( "published", Encode.bool setting.published )
    , ( "order_limit", Encode.string order_limit )
    , ( "daily_limit", Encode.string daily_limit )
    ]


decoder =
    succeed PostResponse
        |> required "success" bool
        |> required "path" string


set_command msg model =
    case msg of
        SelectOutletSetting _ ->
            ( model, focus_on "menu_remarks" )

        BackToEdit ->
            ( model, focus_on "menu_remarks" )

        RevertEdits idx ->
            let
                dom_id =
                    build_dom_id idx "undo"
            in
            ( model, Task.attempt (\_ -> NoOp) (Dom.blur dom_id) )

        RedoEdits idx ->
            let
                dom_id =
                    build_dom_id idx "redo"
            in
            ( model, Task.attempt (\_ -> NoOp) (Dom.blur dom_id) )

        _ ->
            ( model, Cmd.none )


set_template strIndex model =
    let
        form =
            model.form

        setting =
            List.Extra.getAt (Maybe.withDefault 0 (String.toInt strIndex)) model.form.product_settings

        updated_model =
            { model | form = { form | template = setting } }
    in
    { updated_model | mode = TemplateEditMode }


apply_template model =
    let
        form =
            model.form

        updated_model =
            apply_edits_to_product_settings model
    in
    { updated_model | mode = TemplateReviewMode }


apply_edits_to_product_settings model =
    let
        form =
            model.form

        updated_product_settings =
            List.map (apply_edits_to_product_setting model.form.template) model.form.product_settings
    in
    { model | form = { form | product_settings = updated_product_settings } }


unset_template model =
    let
        form =
            model.form

        updated_model =
            { model | form = { form | template = Nothing } }
    in
    { updated_model | mode = TemplateSelectMode }


reset_product_settings model =
    let
        form =
            model.form
    in
    { model | form = { form | product_settings = model.flags.product.product_settings } }


apply_edits_to_product_setting template product_setting =
    let
        tpl =
            case template of
                Just value ->
                    value

                _ ->
                    product_setting
    in
    { id = product_setting.id
    , shop_id = product_setting.shop_id
    , menu_remarks = tpl.menu_remarks
    , status = tpl.status
    , enabled = tpl.enabled
    , enabled_for_menu = tpl.enabled_for_menu
    , published = tpl.published
    , order_limit = tpl.order_limit
    , daily_limit = tpl.daily_limit
    }


back_to_edit_template model =
    { model | mode = TemplateEditMode }


revert_edits idx model =
    let
        form =
            model.form

        try_setting =
            List.Extra.getAt idx model.flags.product.product_settings
    in
    case try_setting of
        Just setting ->
            let
                updated_product_settings =
                    List.Extra.updateAt idx (return_this setting) model.form.product_settings
            in
            { model | form = { form | product_settings = updated_product_settings } }

        _ ->
            model


redo_edits idx model =
    let
        form =
            model.form
    in
    case List.Extra.getAt idx model.form.product_settings of
        Just product_setting ->
            let
                updated_product_setting =
                    apply_edits_to_product_setting model.form.template product_setting

                updated_product_settings =
                    List.Extra.updateAt idx (return_this updated_product_setting) model.form.product_settings
            in
            { model | form = { form | product_settings = updated_product_settings } }

        _ ->
            model


return_this setting _ =
    setting


focus_on dom_id =
    Task.attempt (\_ -> NoOp) (Dom.focus dom_id)


update_model msg model =
    model
        |> update_form_data msg


update_form_data : Msg -> PsModel -> PsModel
update_form_data msg model =
    case model.form.template of
        Just template ->
            let
                form =
                    model.form
            in
            case msg of
                UpdateMenuRemarks strValue ->
                    { model
                        | form = { form | template = Just { template | menu_remarks = strValue } }
                    }

                UpdateStatus strValue ->
                    { model
                        | form = { form | template = Just { template | status = strValue } }
                    }

                UpdateDailyLimit strValue ->
                    { model
                        | form = { form | template = Just { template | daily_limit = String.toInt strValue } }
                    }

                UpdateOrderLimit strValue ->
                    { model
                        | form = { form | template = Just { template | order_limit = String.toInt strValue } }
                    }

                TogglePublished ->
                    { model
                        | form = { form | template = Just { template | published = toggle_template_checkbox .published model } }
                    }

                ToggleEnabled ->
                    { model
                        | form = { form | template = Just { template | enabled = toggle_template_checkbox .enabled model } }
                    }

                ToggleEnabledForMenu ->
                    { model
                        | form = { form | template = Just { template | enabled_for_menu = toggle_template_checkbox .enabled_for_menu model } }
                    }

                _ ->
                    model

        _ ->
            model


toggle_template_checkbox fcn model =
    case model.form.template of
        Just template ->
            not (fcn template)

        _ ->
            False
