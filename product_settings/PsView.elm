module PsView exposing (view)

import Hotkeys exposing (onKeyCodes)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra exposing (..)
import PsModel exposing (..)


onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
    Html.Events.custom "click" (Json.Decode.succeed { message = message, stopPropagation = True, preventDefault = True })


view : PsModel -> Html Msg
view model =
    div [] [ settings_list model ]


settings_list model =
    div []
        [ instruction model
        , template_view model
        , report model
        ]


template_view model =
    case model.mode of
        TemplateSelectMode ->
            div [] []

        TemplateEditMode ->
            template_form model

        TemplateReviewMode ->
            div [] []


template_form model =
    div []
        [ Html.form
            [ class "formtastic admin_user" ]
            [ fieldset [ class "inputs" ]
                [ ol []
                    [ li [ class "string input stringish" ]
                        [ label [ class "label", for "menu_remarks" ] [ text "Menu remarks" ]
                        , input
                            [ id "menu_remarks"
                            , type_ "text"
                            , value (template_try_string_value .menu_remarks model)
                            , onInput UpdateMenuRemarks
                            , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
                            ]
                            []
                        ]
                    , li [ class "string input stringish" ]
                        [ label [ class "label", for "status" ] [ text "Status" ]
                        , input
                            [ id "status"
                            , type_ "text"
                            , value (template_try_string_value .status model)
                            , onInput UpdateStatus
                            , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
                            ]
                            []
                        ]
                    , li [ class "number input numeric stringish" ]
                        [ label [ class "label", for "daily_limit" ] [ text "Daily limit" ]
                        , input
                            [ id "daily_limit"
                            , type_ "number"
                            , value (template_try_int_value .daily_limit model)
                            , onInput UpdateDailyLimit
                            , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
                            ]
                            []
                        ]
                    , li [ class "number input numeric stringish" ]
                        [ label [ class "label", for "order_limit" ] [ text "Order limit" ]
                        , input
                            [ id "order_limit"
                            , type_ "number"
                            , value (template_try_int_value .order_limit model)
                            , onInput UpdateOrderLimit
                            , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
                            ]
                            []
                        ]
                    , li [ class "boolean input" ]
                        [ label [ class "", for "published" ]
                            [ checkbox "published" .published TogglePublished model
                            , text "Published"
                            ]
                        ]
                    , li [ class "boolean input" ]
                        [ label [ class "", for "enabled" ]
                            [ checkbox "enabled" .enabled ToggleEnabled model
                            , text "Enabled"
                            ]
                        ]
                    , li [ class "boolean input" ]
                        [ label [ class "", for "enabled_for_menu" ]
                            [ checkbox "enabled_for_menu" .enabled_for_menu ToggleEnabledForMenu model
                            , text "Enabled for menu"
                            ]
                        ]
                    ]
                ]
            , fieldset [ class "actions" ]
                [ ol []
                    [ li [] [ a [ class "button ok-btn", onClick ProceedEdit ] [ text "Proceed" ] ]
                    , li [] [ a [ class "cancel-btn", onClick CancelEdit ] [ text "Cancel" ] ]
                    ]
                ]
            ]
        ]


checkbox cid fcn toggle_msg model =
    let
        bool =
            case model.form.template of
                Just template ->
                    fcn template

                _ ->
                    False
    in
    input
        [ id cid
        , type_ "checkbox"
        , checked <| bool
        , onClick <| toggle_msg
        ]
        []


template_try_string_value fcn model =
    case model.form.template of
        Just template ->
            fcn template

        _ ->
            ""


template_try_int_value fcn model =
    case model.form.template of
        Just template ->
            case fcn template of
                Just value ->
                    String.fromInt value

                _ ->
                    ""

        _ ->
            ""


instruction model =
    let
        message =
            case model.mode of
                TemplateSelectMode ->
                    "Select a template"

                TemplateEditMode ->
                    "Edit Product Setting"

                TemplateReviewMode ->
                    if model.flags.product.product_settings == model.form.product_settings then
                        "No changes"

                    else
                        "Review Edits"
    in
    h2 [ class "ta-header" ] [ text message ]


report model =
    case model.mode of
        TemplateSelectMode ->
            render_report model

        TemplateEditMode ->
            div [] []

        TemplateReviewMode ->
            render_report model


render_report model =
    let
        product_settings =
            model.form.product_settings
    in
    case List.length product_settings of
        0 ->
            label [] [ text "No outlet settings" ]

        _ ->
            case model.mode of
                TemplateReviewMode ->
                    div []
                        [ table [ class "ta-table product-setting-table" ]
                            [ table_header model
                            , tbody [] (product_settings |> List.indexedMap (\idx setting -> report_row idx setting model))
                            ]
                        , div [] (nav_buttons model)
                        ]

                _ ->
                    div []
                        [ table [ class "ta-table product-setting-table" ]
                            [ table_header model
                            , tbody [] (product_settings |> List.indexedMap (\idx setting -> report_row idx setting model))
                            ]
                        ]


nav_buttons model =
    if model.flags.product.product_settings == model.form.product_settings then
        [ a [ class "cancel-btn", onClick BackToEdit ] [ text "Back to Edit" ]
        ]

    else
        [ a [ class "button ok-btn", onClick Submit ] [ text "Submit" ]
        , a [ class "cancel-btn", onClick BackToEdit ] [ text "Back to Edit" ]
        ]


table_header model =
    let
        trs =
            case model.mode of
                TemplateReviewMode ->
                    [ td [] [ text "" ]
                    , td [] [ text "menu remarks" ]
                    , td [] [ text "status" ]
                    , td [] [ text "daily limit" ]
                    , td [] [ text "order limit" ]
                    , td [] [ text "published" ]
                    , td [] [ text "enabled" ]
                    , td [] [ text "enabled for menu" ]
                    , td [] [ text "action" ]
                    ]

                _ ->
                    [ td [] [ text "" ]
                    , td [] [ text "menu remarks" ]
                    , td [] [ text "status" ]
                    , td [] [ text "daily limit" ]
                    , td [] [ text "order limit" ]
                    , td [] [ text "published" ]
                    , td [] [ text "enabled" ]
                    , td [] [ text "enabled for menu" ]
                    ]
    in
    thead [] [ tr [] trs ]


report_row idx setting model =
    let
        trs =
            case model.mode of
                TemplateReviewMode ->
                    [ td [] [ div [ class "ta-user-type" ] [ text (shop_name setting.shop_id model) ] ]
                    , td [] [ render_comparison idx .menu_remarks model ]
                    , td [] [ render_comparison idx .status model ]
                    , td [ class "text-center" ] [ render_comparison_int idx .daily_limit model ]
                    , td [ class "text-center" ] [ render_comparison_int idx .order_limit model ]
                    , td [ class "text-center" ] [ render_comparison_bool idx .published model ]
                    , td [ class "text-center" ] [ render_comparison_bool idx .enabled model ]
                    , td [ class "text-center" ] [ render_comparison_bool idx .enabled_for_menu model ]
                    , td [ class "text-center" ] [ render_action idx model ]
                    ]

                _ ->
                    [ td [] [ div [ class "ta-user-type" ] [ text (shop_name setting.shop_id model) ] ]
                    , td [] [ text setting.menu_remarks ]
                    , td [] [ text setting.status ]
                    , td [ class "text-center" ] [ render_maybe_int setting.daily_limit ]
                    , td [ class "text-center" ] [ render_maybe_int setting.order_limit ]
                    , td [ class "text-center" ] [ text (bool_yes_no setting.published) ]
                    , td [ class "text-center" ] [ text (bool_yes_no setting.enabled) ]
                    , td [ class "text-center" ] [ text (bool_yes_no setting.enabled_for_menu) ]
                    ]
    in
    tr (tr_attributes idx model) trs


render_action idx model =
    let
        try_old =
            List.Extra.getAt idx model.flags.product.product_settings

        try_new =
            List.Extra.getAt idx model.form.product_settings
    in
    if equal_editables try_new model.form.template then
        if try_old == try_new then
            div [] []

        else
            button
                [ id (build_dom_id idx "redo")
                , class "cancel-btn btn-light-pointer"
                , onClickNoBubble (RevertEdits idx)
                ]
                [ text "↺" ]

    else
        button
            [ id (build_dom_id idx "undo")
            , class "btn-light-pointer"
            , onClickNoBubble (RedoEdits idx)
            ]
            [ text "↻" ]


equal_editables try_a try_b =
    case ( try_a, try_b ) of
        ( Nothing, Nothing ) ->
            True

        ( Just a, Just b ) ->
            a.menu_remarks
                == b.menu_remarks
                && a.status
                == b.status
                && a.enabled
                == b.enabled
                && a.enabled_for_menu
                == b.enabled_for_menu
                && a.published
                == b.published
                && a.order_limit
                == b.order_limit
                && a.daily_limit
                == b.daily_limit

        _ ->
            False


render_comparison idx fcn model =
    let
        old =
            get_old_string idx fcn model

        new =
            get_new_string idx fcn model
    in
    div []
        [ div [ class "one-line", class (class_old old new) ] [ text old ]
        , div [ class "one-line compare-new-updated" ] [ text new ]
        ]


render_comparison_bool idx fcn model =
    let
        old =
            get_old_bool idx fcn model

        new =
            get_new_bool idx fcn model
    in
    div []
        [ div [ class "one-line", class (class_old old new) ] [ text old ]
        , div [ class "one-line compare-new-updated" ] [ text new ]
        ]


render_comparison_int idx fcn model =
    let
        old =
            get_old_int idx fcn model

        new =
            get_new_int idx fcn model
    in
    div []
        [ div [ class "one-line", class (class_old old new) ] [ text old ]
        , div [ class "one-line compare-new-updated" ] [ text new ]
        ]


render_daily_limit idx setting model =
    div []
        [ div [ class "one-line" ] [ render_maybe_int setting.daily_limit ]
        , div [] [ render_maybe_int setting.daily_limit ]
        ]


render_order_limit idx setting model =
    div []
        [ div [ class "one-line" ] [ render_maybe_int setting.order_limit ]
        , div [] [ render_maybe_int setting.order_limit ]
        ]


render_published idx setting model =
    div []
        [ text (bool_yes_no setting.published)
        , text (bool_yes_no setting.published)
        ]


render_enabled idx setting model =
    div []
        [ text (bool_yes_no setting.enabled)
        , text (bool_yes_no setting.enabled)
        ]


render_enabled_for_menu idx setting model =
    div []
        [ text (bool_yes_no setting.enabled_for_menu)
        , text (bool_yes_no setting.enabled_for_menu)
        ]


get_old_string idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.flags.product.product_settings
    in
    get_string fcn try_setting


get_new_string idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.form.product_settings
    in
    get_string fcn try_setting


get_old_int idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.flags.product.product_settings
    in
    get_int fcn try_setting


get_new_int idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.form.product_settings
    in
    get_int fcn try_setting


get_old_bool idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.flags.product.product_settings
    in
    case try_setting of
        Just setting ->
            bool_yes_no (fcn setting)

        _ ->
            ""


get_new_bool idx fcn model =
    let
        try_setting =
            List.Extra.getAt idx model.form.product_settings
    in
    case try_setting of
        Just setting ->
            bool_yes_no (fcn setting)

        _ ->
            ""


class_old old new =
    if old == new then
        ""

    else
        "compare-old"


get_string fcn try_setting =
    case try_setting of
        Just setting ->
            fcn setting

        _ ->
            ""


get_int fcn try_setting =
    case try_setting of
        Just setting ->
            case fcn setting of
                Just val ->
                    String.fromInt val

                _ ->
                    ""

        _ ->
            ""


tr_attributes idx model =
    case model.mode of
        TemplateSelectMode ->
            [ class (tr_class idx model)
            , onClickNoBubble (SelectOutletSetting (String.fromInt idx))
            , id ("report-tr-" ++ String.fromInt (1000 + idx))
            ]

        _ ->
            [ class (tr_class idx model)
            , id ("report-tr-" ++ String.fromInt (1000 + idx))
            ]


bool_yes_no val =
    if val then
        "YES"

    else
        "NO"


render_maybe_int any =
    text <|
        case any of
            Just value ->
                String.fromInt value

            _ ->
                ""


shop_name id model =
    let
        list =
            List.filter (\id_name -> Tuple.first id_name == id) model.flags.shops
    in
    case List.head list of
        Just id_name ->
            Tuple.second id_name

        _ ->
            "Unknown shop"


tr_class idx model =
    String.join " "
        [ tr_class_mode idx model
        ]


tr_class_mode idx model =
    case model.mode of
        TemplateSelectMode ->
            "tr-selectable"

        TemplateEditMode ->
            ""

        TemplateReviewMode ->
            ""
