module View exposing (userable_selection, view)

import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Hotkeys exposing (onKeyCodes)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Number as Number
import Json.Decode
import Model exposing (..)
import String.Extra


onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
    Html.Events.custom "click" (Json.Decode.succeed { message = message, stopPropagation = True, preventDefault = True })


view : Model -> Html Msg
view model =
    case model.view of
        ViewUserableOptions ->
            render userable_selection model

        ViewUnregisteredUserOptions ->
            render unregistered_users model

        ViewMemberOptions ->
            render members model

        ViewRegistrationDateRangeForm ->
            render registration_date model

        ViewNotActiveForAtLeastXDaysForm ->
            render not_active_for_at_least_x_days model

        ViewActiveInTheLastXDaysForm ->
            render active_in_the_last_x_days model

        ViewPaidPurchaseActivityInXDaysForm ->
            render members model

        ViewHasNotPurchasedInXDaysForm ->
            render has_not_purchased_in_x_days model

        ViewHasPurchasedInXDaysForm ->
            render has_purchased_in_x_days model

        ViewPromotionUsageForm ->
            render members model

        ViewHasNotUsedPromotionForm ->
            render has_not_used_promotion model

        ViewHasUsedPromotionForm ->
            render has_used_promotion model

        ViewVoucherUsageForm ->
            render members model

        ViewHasVoucher ->
            render has_voucher model

        ViewHasVoucherButNeverUsedForm ->
            render has_voucher_but_never_used model

        ViewHasUsedTheVoucherForm ->
            render has_used_the_voucher model

        ViewDoesNotHaveTheVoucherForm ->
            render does_not_have_the_voucher model

        ViewPaidPurchaseCountForm ->
            render paid_purchase_count model

        ViewBasketValueRangeForm ->
            render basket_value_range model

        ViewBasketQuantityRangeForm ->
            render basket_quantity_range model

        ViewSubmitForm ->
            render submit_form model


members model =
    div []
        [ question [] [ text "What condition would you like to apply?" ]
        , menu_list
            [ menu_option [ onClick SelectRegistrationDateRange ] [ text "Registration date" ]
            , menu_option [ onClick SelectNotActiveForAtLeastXDays ] [ text "Not active for at least x days" ]
            , select_paid_purchase_activity_in_x_days_options model
            , select_active_in_the_last_x_days_options model
            , menu_option [ onClick SelectPaidPurchaseCount ] [ text "Paid purchase count" ]
            , select_promotion_usage_options model
            , select_voucher_usage_options model
            , menu_option [ onClick SelectBasketValueRange ] [ text "Basket value" ]
            , menu_option [ onClick SelectBasketQuantityRange ] [ text "Basket average quantity" ]
            ]
        , back model
        ]


select_active_in_the_last_x_days_options model =
    let
        menu =
            "Browsing activity"
    in
    case model.prev_action of
        Just SelectBrowsingActivityForAtLeastXDays ->
            div []
                [ menu_option [ onClick SelectMembers ] [ text menu ]
                , submenu_list
                    [ menu_option [ onClick SelectNotActiveForAtLeastXDays ] [ text "NOT active for at least x days" ]
                    , menu_option [ onClick SelectActiveInTheLastXDays ] [ text "Active in the last x days" ]
                    ]
                ]

        _ ->
            menu_option [ onClick SelectBrowsingActivityForAtLeastXDays ] [ text menu ]


select_paid_purchase_activity_in_x_days_options model =
    case model.prev_action of
        Just SelectPaidPurchaseActivityInXDays ->
            div []
                [ menu_option [ onClick SelectMembers ] [ text "Paid purchase activity in x days" ]
                , submenu_list
                    [ menu_option [ onClick SelectHasNotPurchasedInXDays ] [ text "Has NOT purchased in x days" ]
                    , menu_option [ onClick SelectHasPurchasedInXDays ] [ text "Has purchased in x days" ]
                    ]
                ]

        _ ->
            menu_option [ onClick SelectPaidPurchaseActivityInXDays ] [ text "Paid purchase activity in x days" ]


select_promotion_usage_options model =
    case model.prev_action of
        Just SelectPromotionUsage ->
            div []
                [ menu_option [ onClick SelectMembers ] [ text "Promotion usage" ]
                , submenu_list
                    [ menu_option [ onClick SelectHasNotUsedPromotion ] [ text "Has NOT used promotion" ]
                    , menu_option [ onClick SelectHasUsedPromotion ] [ text "Has used promotion" ]
                    ]
                ]

        _ ->
            menu_option [ onClick SelectPromotionUsage ] [ text "Promotion usage" ]


select_voucher_usage_options model =
    case model.prev_action of
        Just SelectVoucherUsage ->
            div []
                [ menu_option [ onClick SelectMembers ] [ text "Voucher" ]
                , submenu_list
                    [ menu_option [ onClick SelectDoesNotHaveTheVoucher ] [ text "Does NOT have the voucher" ]
                    , menu_option [ onClick SelectHasVoucher ] [ text "Has voucher regardless of usage" ]
                    , menu_option [ onClick SelectHasVoucherButNeverUsed ] [ text "Has voucher but never used" ]
                    , menu_option [ onClick SelectHasUsedTheVoucher ] [ text "Has used the voucher" ]
                    ]
                ]

        _ ->
            menu_option [ onClick SelectVoucherUsage ] [ text "Voucher" ]


nothing_here model =
    div []
        [ p [] [ text "Nothing to show..." ]
        , back model
        ]


render fx model =
    let
        actions_ =
            case List.length model.criteria of
                0 ->
                    div [] []

                _ ->
                    case model.view of
                        ViewSubmitForm ->
                            div []
                                [ button_back_to SelectUserable
                                , button [ onClick Submit ] [ text "Submit" ]
                                ]

                        ViewUserableOptions ->
                            button [ onClick Proceed ] [ text "Proceed" ]

                        ViewUnregisteredUserOptions ->
                            button [ onClick Proceed ] [ text "Proceed" ]

                        ViewMemberOptions ->
                            button [ onClick Proceed ] [ text "Proceed" ]

                        _ ->
                            case model.form.edit_idx of
                                Just idx_str ->
                                    case String.toInt idx_str of
                                        Just idx_int ->
                                            if idx_int >= 1000 then
                                                button [ onClick Proceed ] [ text "Proceed" ]

                                            else
                                                div [] []

                                        _ ->
                                            div [] []

                                _ ->
                                    div [] []
    in
    div []
        [ header model
        , show_errors model
        , fx model
        , criteria_list model
        , actions_
        ]


show_errors model =
    div [ class "ta-errors" ]
        [ model.form.errors
            |> List.map (\l -> li [] [ text l ])
            |> ul []
        ]


criteria_list model =
    div [ style "margin-top" "50px" ]
        [ h2 [ class "ta-header" ] [ text "Criteria" ]
        , report model
        ]


report model =
    let
        criteria =
            model.criteria
    in
    case List.length criteria of
        0 ->
            label [] [ text "No criteria" ]

        _ ->
            criteria
                |> List.indexedMap
                    (\idx condition ->
                        tr
                            [ class (tr_class idx model)
                            , onClickNoBubble (SelectCondition (String.fromInt idx))
                            , onKeyCodes
                                [ ( 27, ClearSelectCondition )
                                , ( 13, EditSelectedCondition )
                                , ( 38, UpSelectCondition )
                                , ( 40, DownSelectCondition )
                                , ( 32, EditSelectedCondition )
                                ]
                            , tabindex 0
                            , id ("report-tr-" ++ String.fromInt (1000 + idx))
                            ]
                            [ td []
                                [ div [ class "ta-user-type" ] [ text condition.user_type ]
                                , div [ class "ta-condition-name" ]
                                    [ text (display_condition_name condition ++ ": ")
                                    , span [ class "ta-condition-value" ]
                                        [ text (main_condition_value model condition.details)
                                        ]
                                    ]
                                , subcondition_list model condition.details
                                ]
                            , td [ class "ta-action" ]
                                [ button [ onClickNoBubble (EditCondition (String.fromInt idx)) ] [ text "_" ]
                                , button [ onClickNoBubble (RemoveCondition (String.fromInt idx)) ] [ text "×" ]
                                ]
                            ]
                    )
                |> table [ class "ta-table" ]


tr_class idx model =
    String.join " "
        [ tr_class_when_newly_added idx model
        , tr_class_hilite idx model
        ]


tr_class_when_newly_added idx model =
    if model.criteria_updated && idx == 0 then
        "new-entry"

    else
        ""


tr_class_hilite idx model =
    case model.form.edit_idx of
        Just edit_idx ->
            case String.toInt edit_idx of
                Just idx_ ->
                    if idx_ >= 1000 then
                        if remainderBy 1000 idx_ == idx then
                            "on-select"

                        else
                            ""

                    else if idx_ == idx then
                        "on-edit"

                    else
                        ""

                _ ->
                    ""

        _ ->
            ""


main_condition_value model details =
    case List.head details of
        Just condition ->
            display_condition_value model condition

        _ ->
            ""


subcondition_list model subconditions =
    let
        cond_list =
            case List.tail subconditions of
                Just [] ->
                    []

                Just tail ->
                    tail

                _ ->
                    []
    in
    cond_list
        |> List.map (\item -> subcondition_list_li item model)
        |> ul [ class "ul-conditions" ]


subcondition_list_li condition model =
    case condition.key of
        "has_not_used_promotion_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        "has_used_promotion_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        "has_voucher_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        "has_voucher_but_never_used_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        "has_used_the_voucher_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        "does_not_have_the_voucher_id" ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]

        _ ->
            li []
                [ text (display_condition_key_name condition ++ ": ")
                , span [ class "ta-condition-value" ] [ text (display_condition_value model condition) ]
                ]


display_condition_name condition =
    (case condition.name of
        "has_not_used_promotion_id" ->
            "has_not_used_promotion"

        "has_used_promotion_id" ->
            "has_used_promotion"

        "has_voucher_id" ->
            "has_voucher"

        "has_voucher_but_never_used_id" ->
            "has_voucher_but_never_used"

        "has_used_the_voucher_id" ->
            "has_used_the_voucher"

        "does_not_have_the_voucher_id" ->
            "does_not_have_the_voucher"

        _ ->
            condition.name
    )
        |> String.replace "_" " "
        |> String.Extra.toSentenceCase


display_condition_key_name condition =
    condition.key
        |> String.replace "_" " "
        |> String.Extra.toSentenceCase


display_condition_value model condition =
    case condition.key of
        "has_not_used_promotion_id" ->
            let
                promotions =
                    model.flags.promotions
            in
            tuple_select_text promotions condition.value

        "has_used_promotion_id" ->
            let
                promotions =
                    model.flags.promotions
            in
            tuple_select_text promotions condition.value

        "has_voucher_id" ->
            let
                vouchers =
                    model.flags.vouchers
            in
            tuple_select_text vouchers condition.value

        "has_voucher_but_never_used_id" ->
            let
                vouchers =
                    model.flags.vouchers
            in
            tuple_select_text vouchers condition.value

        "has_used_the_voucher_id" ->
            let
                vouchers =
                    model.flags.vouchers
            in
            tuple_select_text vouchers condition.value

        "does_not_have_the_voucher_id" ->
            let
                vouchers =
                    model.flags.vouchers
            in
            tuple_select_text vouchers condition.value

        _ ->
            condition.value


tuple_select_text tuple_selections value =
    let
        int_val =
            Maybe.withDefault 0 (String.toInt value)
    in
    tuple_selections
        |> List.filterMap
            (\( id_, name_ ) ->
                if id_ == int_val then
                    Just name_

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault "?"


userable_selection model =
    div []
        [ question [] [ text "What user type would you like to target?" ]
        , menu_list
            [ menu_option [ onClick SelectUnregisteredUsers ] [ text "Unregistered users" ]
            , menu_option [ onClick SelectMembers ] [ text "Members" ]
            ]
        ]


menu_list contents =
    div [ style "margin-top" "20px" ] contents


submenu_list list =
    div [ style "margin-left" "20px" ] list


unregistered_users model =
    div []
        [ question [] [ text "What condition would you like to apply?" ]
        , menu_list
            [ menu_option [ onClick SelectNotActiveForAtLeastXDays ] [ text "NOT active for at least x days" ]
            , menu_option [ onClick SelectActiveInTheLastXDays ] [ text "Active in the last x days" ]
            ]
        , back model
        ]


not_active_for_at_least_x_days model =
    let
        form =
            model.form

        x_days =
            case form.userable of
                UnregisteredUsers ->
                    form.unregistered_users.not_active_for_at_least_x_days

                Members ->
                    form.members.not_active_for_at_least_x_days

                _ ->
                    ""
    in
    number_input_group_with_help model "Number of days" x_days UpdateNotActiveForAtLeastXDays "not_active_for_at_least_x_days" "Enter zero to target all"


active_in_the_last_x_days model =
    let
        form =
            model.form

        x_days =
            case form.userable of
                UnregisteredUsers ->
                    form.unregistered_users.active_in_the_last_x_days

                Members ->
                    form.members.active_in_the_last_x_days

                _ ->
                    ""
    in
    number_input_group_with_help model "Number of days" x_days UpdateActiveInTheLastXDays "active_in_the_last_x_days" ""


has_not_purchased_in_x_days model =
    let
        form =
            model.form
    in
    number_input_group model
        "Number of days"
        form.members.has_not_purchased_in_x_days
        UpdateHasNotPurchasedInXDays
        "has_not_purchased_in_x_days"


has_purchased_in_x_days model =
    let
        form =
            model.form
    in
    number_input_group model
        "Number of days"
        form.members.has_purchased_in_x_days
        UpdateHasPurchasedInXDays
        "has_purchased_in_x_days"


paid_purchase_count model =
    let
        form =
            model.form
    in
    div []
        [ question [] [ text "" ]
        , selection model
            .paid_purchase_count_comparison_operator
            SelectPaidPurchaseCountComparisonOperator
            "paid_purchase_count_comparison_operator"
            [ ( "", "" )
            , ( "<", "Less than" )
            , ( "<=", "Less than or equal to" )
            , ( "==", "Equal to" )
            , ( ">=", "Greater than or equal to" )
            , ( ">", "Greater than" )
            ]
        , number_input [ id "paid_purchase_count", onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ] ]
            form.members.paid_purchase_count
            UpdatePaidPurchaseCount
        , div [ class "help-text" ] [ text "Enter zero to target members with no paid order" ]
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


registration_date model =
    let
        form =
            model.form
    in
    div []
        [ question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


basket_value_range model =
    let
        form =
            model.form
    in
    div []
        [ div []
            [ selection model
                .basket_value_comparison_operator
                SelectBasketValueComparisonOperator
                "basket_value_comparison_operator"
                [ ( "", "" )
                , ( "<", "Less than" )
                , ( "<=", "Less than or equal to" )
                , ( "==", "Equal to" )
                , ( ">=", "Greater than or equal to" )
                , ( ">", "Greater than" )
                ]
            , number_input [ id "basket_value", onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ] ]
                form.members.basket_value
                UpdateBasketValue
            , question [] [ text "Orders aggregation" ]
            , selection model
                .basket_value_aggregation
                SelectBasketValueAggregation
                "basket_value_aggregation"
                [ ( "", "" )
                , ( "AVERAGE", "Average" )
                , ( "NO_AGGREGATION_ANY_ORDER", "No aggregation, at least one" )
                ]
            ]
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


basket_quantity_range model =
    let
        form =
            model.form
    in
    div []
        [ div []
            [ question [] [ text "" ]
            , selection model
                .basket_quantity_comparison_operator
                SelectBasketQuantityComparisonOperator
                "basket_quantity_comparison_operator"
                [ ( "", "" )
                , ( "<", "Less than" )
                , ( "<=", "Less than or equal to" )
                , ( "==", "Equal to" )
                , ( ">=", "Greater than or equal to" )
                , ( ">", "Greater than" )
                ]
            , number_input [ id "basket_quantity", onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ] ]
                form.members.basket_quantity
                UpdateBasketQuantity
            ]
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


submit_form model =
    div [ class "submit-container-div" ]
        [ question [] [ text "Name" ]
        , input
            [ id "ta_name"
            , type_ "text"
            , value model.form.name
            , onInput UpdateTemplateName
            , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
            ]
            []
        , question [] [ text "Description" ]
        , textarea
            [ id "ta_description"
            , rows 4
            , cols 100
            , value model.form.description
            , onInput UpdateTemplateDescription
            ]
            []
        , and_or_prompt model
        ]


and_or_prompt model =
    case List.length model.criteria of
        1 ->
            div [] []

        _ ->
            div []
                [ question [] [ text "How would you like to apply the criteria below?" ]
                , div []
                    [ label [ class "ta-option" ]
                        [ input
                            [ type_ "radio"
                            , name "and_or"
                            , value "&&"
                            , onInput SelectAndOrCondition
                            , checked (model.form.and_or == "&&")
                            ]
                            []
                        , span [ style "padding-left" "10px" ] [ text "ALL, this will yield less result" ]
                        ]
                    ]
                , div []
                    [ label [ class "ta-option" ]
                        [ input
                            [ type_ "radio"
                            , name "and_or"
                            , value "||"
                            , onInput SelectAndOrCondition
                            , checked (model.form.and_or == "||")
                            ]
                            []
                        , span [ style "padding-left" "10px" ] [ text "ANY, this will yield more result" ]
                        ]
                    ]
                ]


has_not_used_promotion model =
    div []
        [ question [] [ text "Promotion" ]
        , model.flags.promotions
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .has_not_used_promotion_id
                SelectHasNotUsedPromotionId
                "has_not_used_promotion_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


has_used_promotion model =
    div []
        [ question [] [ text "Promotion" ]
        , model.flags.promotions
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .has_used_promotion_id
                SelectHasUsedPromotionId
                "has_used_promotion_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


has_voucher model =
    div []
        [ question [] [ text "Voucher" ]
        , model.flags.vouchers
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .has_voucher_id
                SelectHasVoucherId
                "has_voucher_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


has_voucher_but_never_used model =
    div []
        [ question [] [ text "Voucher" ]
        , model.flags.vouchers
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .has_voucher_but_never_used_id
                SelectHasVoucherButNeverUsedId
                "has_voucher_but_never_used_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


has_used_the_voucher model =
    div []
        [ question [] [ text "Voucher" ]
        , model.flags.vouchers
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .has_used_the_voucher_id
                SelectHasUsedTheVoucherId
                "has_used_the_voucher_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


does_not_have_the_voucher model =
    div []
        [ question [] [ text "Voucher" ]
        , model.flags.vouchers
            |> stringify_options
            |> select_add_empty_first_option
            |> selection model
                .does_not_have_the_voucher_id
                SelectDoesNotHaveTheVoucherId
                "does_not_have_the_voucher_id"
        , question [] [ text "Date range" ]
        , div []
            [ datepicker_input model datepicker_low_settings .date_low .datepicker_low SelectDateLow
            , datepicker_input model datepicker_high_settings .date_high .datepicker_high SelectDateHigh
            ]
        , confirm_answer model
        ]


stringify_options options =
    options
        |> List.map (\item -> ( String.fromInt (Tuple.first item), Tuple.second item ))


select_add_empty_first_option options =
    ( "", "" ) :: options


selection model key action dom_id options =
    select
        [ onInput action
        , id dom_id
        , onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ]
        ]
        (selection_options model options key action)


selection_options model options key action =
    List.map (\item -> select_option item model key) options


select_option item model key =
    let
        id =
            Tuple.first item

        label =
            Tuple.second item

        form =
            model.form

        selected_value =
            key form.members
    in
    if id == selected_value then
        option [ value id, selected True ] [ text label ]

    else
        option [ value id ] [ text label ]


datepicker_input model settings date_key datepicker_key action =
    let
        form =
            model.form
    in
    DatePicker.view (date_key form) settings (datepicker_key form)
        |> Html.map action


number_input_group_with_help model label value action dom_id help_text =
    number_input_with_help model label value action "0" help_text dom_id


number_input_group model label value action dom_id =
    number_input_with_help model label value action "0" "" dom_id


number_input_with_help model label value_ action placeholder_ help_text dom_id =
    div []
        [ question [] [ text label ]
        , number_input [ id dom_id, onKeyCodes [ ( 27, EscWasPressed ), ( 13, EnterWasPressed ) ] ] value_ action
        , div [ class "help-text" ] [ text help_text ]
        , confirm_answer model
        ]


number_input attributes value action =
    let
        defaultOptions =
            Number.defaultStringOptions action
    in
    Number.inputString
        { defaultOptions
            | maxLength = Just 4
            , maxValue = Just (365 * 3)
            , minValue = Just 0
        }
        attributes
        value


confirm_answer model =
    actions
        [ action_cancel model
        , action_add model
        ]


actions children =
    div [ style "margin-top" "20px" ] children


action_cancel model =
    button
        [ class "cancel-btn", onClick (cancel_action model) ]
        [ text "Cancel" ]


action_add model =
    let
        caption =
            case edit_idx_int model of
                Just idx_ ->
                    if idx_ < 1000 then
                        "Update Condition"

                    else
                        "Add Condition"

                _ ->
                    "Add Condition"
    in
    button
        [ onClick (add_action model) ]
        [ text caption ]


question attrs contents =
    let
        attributes =
            attrs ++ [ class "ta-question" ]
    in
    h3 attributes contents


header model =
    h2 [ class "ta-header" ] [ text (header_for model) ]


header_for model =
    case model.view of
        ViewUserableOptions ->
            "Start"

        ViewUnregisteredUserOptions ->
            "Unregistered Users"

        ViewMemberOptions ->
            "Members"

        ViewRegistrationDateRangeForm ->
            userable_title model ++ " " ++ "Registration Date"

        ViewNotActiveForAtLeastXDaysForm ->
            userable_title model ++ " " ++ "Not Active For At Least X Days"

        ViewActiveInTheLastXDaysForm ->
            userable_title model ++ " " ++ "Active In The Last X Days"

        ViewHasNotPurchasedInXDaysForm ->
            userable_title model ++ " " ++ "Has NOT Purchased In X Days"

        ViewHasPurchasedInXDaysForm ->
            userable_title model ++ " " ++ "Has Purchased In X Days"

        ViewHasNotUsedPromotionForm ->
            userable_title model ++ " " ++ "Has Not Used Promotion"

        ViewHasUsedPromotionForm ->
            userable_title model ++ " " ++ "Has Used Promotion"

        ViewHasVoucher ->
            userable_title model ++ " " ++ "Has Voucher Regardless of Usage"

        ViewHasVoucherButNeverUsedForm ->
            userable_title model ++ " " ++ "Has Voucher But Never Used"

        ViewHasUsedTheVoucherForm ->
            userable_title model ++ " " ++ "Has Used The Voucher"

        ViewDoesNotHaveTheVoucherForm ->
            userable_title model ++ " " ++ "Does NOT Have The Voucher"

        ViewPaidPurchaseCountForm ->
            userable_title model ++ " " ++ "Paid Purchase Count"

        ViewBasketValueRangeForm ->
            userable_title model ++ " " ++ "Basket Value"

        ViewBasketQuantityRangeForm ->
            userable_title model ++ " " ++ "Basket Average Quantity"

        ViewSubmitForm ->
            "Template Description"

        _ ->
            userable_title model


userable_title model =
    let
        form =
            model.form
    in
    case form.userable of
        UnregisteredUsers ->
            "Unregistered Users"

        Members ->
            "Members"

        _ ->
            ""


menu_option attrs contents =
    let
        attributes =
            attrs ++ [ class "ta-option" ]
    in
    div [ style "margin-top" "5px" ]
        [ label attributes contents
        ]


back model =
    case model.view of
        ViewUnregisteredUserOptions ->
            back_to SelectUserable

        ViewMemberOptions ->
            back_to SelectUserable

        _ ->
            back_from_form model


back_from_form model =
    case model.form.userable of
        UnregisteredUsers ->
            back_to (back_action model)

        Members ->
            back_to (back_action model)

        _ ->
            div [] []


cancel_action model =
    case model.form.userable of
        UnregisteredUsers ->
            CancelAndRedirectToUnregisteredUsers

        _ ->
            CancelAndRedirectToMembers


add_action model =
    case model.form.userable of
        UnregisteredUsers ->
            AddConditionAndRedirectToUnregisteredUsers

        _ ->
            AddConditionAndRedirectToMembers


back_action model =
    case model.form.userable of
        UnregisteredUsers ->
            SelectUnregisteredUsers

        _ ->
            SelectMembers


back_to action =
    div [ style "margin-top" "10px" ] [ button_back_to action ]


button_back_to action =
    button
        [ class "cancel-btn", onClick action ]
        [ text "Back" ]
