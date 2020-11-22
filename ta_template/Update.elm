port module Update exposing (update)

import Browser.Dom as Dom
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug
import Http
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List.Extra exposing (..)
import Model exposing (..)
import Rails
import Task
import Validate


port openWindow : String -> Cmd msg


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCondition strIndex ->
            model
                |> cancel_form msg
                |> hilite_selected_row strIndex
                |> set_command msg

        ClearSelectCondition ->
            clear_select_condition model

        EditSelectedCondition ->
            let
                strIndex =
                    case true_edit_idx model of
                        Just idx_ ->
                            String.fromInt idx_

                        _ ->
                            "0"
            in
            model
                |> clear_errors
                |> edit_form_selector strIndex

        UpSelectCondition ->
            let
                strIndex =
                    case true_edit_idx model of
                        Just idx_ ->
                            if idx_ == 0 then
                                "1000"

                            else
                                String.fromInt (1000 + idx_ - 1)

                        _ ->
                            "1000"
            in
            model
                |> cancel_form msg
                |> hilite_selected_row strIndex
                |> (\model_ ->
                        ( model_, focus_on ("report-tr-" ++ strIndex) )
                   )

        DownSelectCondition ->
            let
                strIndex =
                    case true_edit_idx model of
                        Just idx_ ->
                            let
                                hi =
                                    List.length model.criteria - 1
                            in
                            if idx_ == hi then
                                String.fromInt (1000 + hi)

                            else
                                String.fromInt (1000 + idx_ + 1)

                        _ ->
                            "1000"
            in
            model
                |> cancel_form msg
                |> hilite_selected_row strIndex
                |> (\model_ ->
                        ( model_, focus_on ("report-tr-" ++ strIndex) )
                   )

        EditCondition strIndex ->
            model
                |> clear_errors
                |> edit_form_selector strIndex

        Proceed ->
            model
                |> clear_errors
                |> (\model_ ->
                        ( show_submit_form model_, focus_on "ta_name" )
                   )

        EnterWasPressed ->
            case model.view of
                ViewSubmitForm ->
                    ( model, send Submit )

                _ ->
                    case model.form.userable of
                        UnregisteredUsers ->
                            model
                                |> add_condition_and_redirect_to_unregistered_users
                                |> if_no_error (set_criteria_updated True)
                                |> if_no_error clear_edit_idx
                                |> set_command msg

                        _ ->
                            model
                                |> add_condition_and_redirect_to_members
                                |> if_no_error (set_criteria_updated True)
                                |> if_no_error clear_edit_idx
                                |> set_command msg

        EscWasPressed ->
            model
                |> cancel_form msg
                |> set_command msg

        Submit ->
            submit model

        HandlePostTaTemplateResponse result ->
            handle_post_ta_template_response model result

        HandlePatchTaTemplateResponse result ->
            handle_patch_ta_template_response model result

        HandleGetTaTemplateResponse result ->
            handle_get_ta_template_response model result

        _ ->
            model
                |> update_model msg
                |> keep_prev_action msg
                |> set_command msg


clear_select_condition model =
    let
        row_id =
            case model.form.edit_idx of
                Just edit_idx ->
                    "report-tr-" ++ edit_idx

                _ ->
                    ""
    in
    model
        |> clear_edit_idx
        |> (\model_ ->
                ( model_, Task.attempt (\_ -> NoOp) (Dom.blur row_id) )
           )


cancel_form msg model =
    case model.form.userable of
        UnregisteredUsers ->
            cancel_and_redirect_to_unregistered_users model
                |> clear_errors
                |> keep_prev_action msg

        Members ->
            cancel_and_redirect_to_members model
                |> clear_errors
                |> keep_prev_action msg

        _ ->
            model


keep_prev_action msg model =
    { model | prev_action = Just msg }


set_command msg model =
    case msg of
        SelectRegistrationDateRange ->
            ( model, Cmd.none )

        SelectNotActiveForAtLeastXDays ->
            ( model, focus_on "not_active_for_at_least_x_days" )

        SelectActiveInTheLastXDays ->
            ( model, focus_on "active_in_the_last_x_days" )

        SelectHasNotPurchasedInXDays ->
            ( model, focus_on "has_not_purchased_in_x_days" )

        SelectHasPurchasedInXDays ->
            ( model, focus_on "has_purchased_in_x_days" )

        SelectHasNotUsedPromotion ->
            ( model, focus_on "has_not_used_promotion_id" )

        SelectHasUsedPromotion ->
            ( model, focus_on "has_used_promotion_id" )

        SelectHasVoucher ->
            ( model, focus_on "has_voucher_id" )

        SelectHasVoucherButNeverUsed ->
            ( model, focus_on "has_voucher_but_never_used_id" )

        SelectHasUsedTheVoucher ->
            ( model, focus_on "has_used_the_voucher_id" )

        SelectDoesNotHaveTheVoucher ->
            ( model, focus_on "does_not_have_the_voucher_id" )

        SelectPaidPurchaseCount ->
            ( model, focus_on "paid_purchase_count" )

        SelectBasketValueRange ->
            ( model, focus_on "basket_value" )

        SelectBasketQuantityRange ->
            ( model, focus_on "basket_quantity" )

        AddConditionAndRedirectToUnregisteredUsers ->
            case with_error model of
                True ->
                    case refocus_on_view model.view of
                        Just id ->
                            ( model, focus_on id )

                        _ ->
                            ( model, Cmd.none )

                False ->
                    ( model, focus_on "report-tr-1000" )

        AddConditionAndRedirectToMembers ->
            case with_error model of
                True ->
                    case refocus_on_view model.view of
                        Just id ->
                            ( model, focus_on id )

                        _ ->
                            ( model, Cmd.none )

                False ->
                    ( model, focus_on "report-tr-1000" )

        EnterWasPressed ->
            ( model, focus_on "report-tr-1000" )

        _ ->
            ( model, Cmd.none )


with_error model =
    List.length model.form.errors > 0


refocus_on_view view =
    case view of
        ViewRegistrationDateRangeForm ->
            Nothing

        ViewNotActiveForAtLeastXDaysForm ->
            Just "not_active_for_at_least_x_days"

        ViewActiveInTheLastXDaysForm ->
            Just "active_in_the_last_x_days"

        ViewHasNotPurchasedInXDaysForm ->
            Just "has_not_purchased_in_x_days"

        ViewHasPurchasedInXDaysForm ->
            Just "has_purchased_in_x_days"

        ViewHasNotUsedPromotionForm ->
            Just "has_not_used_promotion_id"

        ViewHasUsedPromotionForm ->
            Just "has_used_promotion_id"

        ViewHasVoucher ->
            Just "has_voucher_id"

        ViewHasVoucherButNeverUsedForm ->
            Just "has_voucher_but_never_used_id"

        ViewHasUsedTheVoucherForm ->
            Just "has_used_the_voucher_id"

        ViewDoesNotHaveTheVoucherForm ->
            Just "does_not_have_the_voucher_id"

        ViewPaidPurchaseCountForm ->
            Just "paid_purchase_count"

        ViewBasketValueRangeForm ->
            Just "basket_value"

        ViewBasketQuantityRangeForm ->
            Just "basket_quantity"

        _ ->
            Nothing


clear_errors model =
    let
        form =
            model.form
    in
    { model | form = { form | errors = [] } }


edit_form_selector strIndex model =
    let
        criteria =
            model.criteria

        idx =
            Maybe.withDefault 0 (String.toInt strIndex)

        condition =
            case List.Extra.getAt idx criteria of
                Just cond_ ->
                    cond_

                _ ->
                    { user_type = "", name = "", details = [] }

        command =
            case condition.name of
                "registration_date" ->
                    SelectRegistrationDateRange

                "not_active_for_at_least_x_days" ->
                    SelectNotActiveForAtLeastXDays

                "active_in_the_last_x_days" ->
                    SelectActiveInTheLastXDays

                "has_not_purchased_in_x_days" ->
                    SelectHasNotPurchasedInXDays

                "has_purchased_in_x_days" ->
                    SelectHasPurchasedInXDays

                "has_not_used_promotion_id" ->
                    SelectHasNotUsedPromotion

                "has_used_promotion_id" ->
                    SelectHasUsedPromotion

                "has_voucher_id" ->
                    SelectHasVoucher

                "has_voucher_but_never_used_id" ->
                    SelectHasVoucherButNeverUsed

                "has_used_the_voucher_id" ->
                    SelectHasUsedTheVoucher

                "does_not_have_the_voucher_id" ->
                    SelectDoesNotHaveTheVoucher

                "paid_purchase_count" ->
                    SelectPaidPurchaseCount

                "basket_value" ->
                    SelectBasketValueRange

                "basket_quantity" ->
                    SelectBasketQuantityRange

                _ ->
                    SelectUserable

        userable =
            case condition.user_type of
                "Unregistered Users" ->
                    UnregisteredUsers

                _ ->
                    Members

        form =
            condition_to_form model condition

        updated_model =
            { model
                | form =
                    { form
                        | userable = userable
                        , edit_idx = Just strIndex
                    }
            }
    in
    ( updated_model, send command )


hilite_selected_row strIndex model =
    let
        form =
            model.form

        edit_idx =
            case String.toInt strIndex of
                Just idx ->
                    Just (String.fromInt (1000 + idx))

                _ ->
                    model.form.edit_idx
    in
    { model | form = { form | edit_idx = edit_idx } }


condition_to_form model condition =
    let
        form =
            model.form

        user_type =
            condition.user_type

        name =
            condition.name

        details =
            condition.details

        members =
            form.members

        unregistered_users =
            form.unregistered_users
    in
    case ( user_type, name ) of
        ( "Unregistered Users", "not_active_for_at_least_x_days" ) ->
            { form
                | unregistered_users =
                    { unregistered_users
                        | not_active_for_at_least_x_days = get_field_value name details
                    }
            }

        ( "Unregistered Users", "active_in_the_last_x_days" ) ->
            { form
                | unregistered_users =
                    { unregistered_users
                        | active_in_the_last_x_days = get_field_value name details
                    }
            }

        ( "Members", "registration_date" ) ->
            { form
                | date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "not_active_for_at_least_x_days" ) ->
            { form
                | members =
                    { members
                        | not_active_for_at_least_x_days = get_field_value name details
                    }
            }

        ( "Members", "active_in_the_last_x_days" ) ->
            { form
                | members =
                    { members
                        | active_in_the_last_x_days = get_field_value name details
                    }
            }

        ( "Members", "has_not_purchased_in_x_days" ) ->
            { form
                | members =
                    { members
                        | has_not_purchased_in_x_days = get_field_value name details
                    }
            }

        ( "Members", "has_purchased_in_x_days" ) ->
            { form
                | members =
                    { members
                        | has_purchased_in_x_days = get_field_value name details
                    }
            }

        ( "Members", "has_not_used_promotion_id" ) ->
            { form
                | members =
                    { members
                        | has_not_used_promotion_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "has_used_promotion_id" ) ->
            { form
                | members =
                    { members
                        | has_used_promotion_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "has_voucher_id" ) ->
            { form
                | members =
                    { members
                        | has_voucher_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "has_voucher_but_never_used_id" ) ->
            { form
                | members =
                    { members
                        | has_voucher_but_never_used_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "has_used_the_voucher_id" ) ->
            { form
                | members =
                    { members
                        | has_used_the_voucher_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "does_not_have_the_voucher_id" ) ->
            { form
                | members =
                    { members
                        | does_not_have_the_voucher_id = get_field_value name details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "paid_purchase_count" ) ->
            { form
                | members =
                    { members
                        | paid_purchase_count = get_field_value name details
                        , paid_purchase_count_comparison_operator = get_field_value "comparison_operator" details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "basket_value" ) ->
            { form
                | members =
                    { members
                        | basket_value = get_field_value name details
                        , basket_value_comparison_operator = get_field_value "comparison_operator" details
                        , basket_value_aggregation = get_field_value "aggregation" details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        ( "Members", "basket_quantity" ) ->
            { form
                | members =
                    { members
                        | basket_quantity = get_field_value name details
                        , basket_quantity_comparison_operator = get_field_value "comparison_operator" details
                    }
                , date_low = get_date_low_field_value details
                , date_high = get_date_high_field_value details
            }

        _ ->
            form


get_date_low_field_value details =
    let
        date =
            get_date_low details
    in
    case Date.fromIsoString date of
        Ok date_ ->
            Just date_

        _ ->
            Nothing


get_date_high_field_value details =
    let
        date =
            get_date_high details
    in
    case Date.fromIsoString date of
        Ok date_ ->
            Just date_

        _ ->
            Nothing


get_date_low details =
    let
        parts =
            String.split " - " (get_inclusive_dates_str details)
    in
    case List.head parts of
        Just date_str ->
            date_str

        _ ->
            ""


get_date_high details =
    let
        parts =
            String.split " - " (get_inclusive_dates_str details)
    in
    case List.tail parts of
        Just tail ->
            case List.head tail of
                Just date_str ->
                    date_str

                _ ->
                    ""

        _ ->
            ""


get_inclusive_dates_str details =
    let
        list =
            List.filter (\p -> p.key == "inclusive_dates") details
    in
    case List.head list of
        Just inclusive_dates ->
            inclusive_dates.value

        _ ->
            ""


get_field_value name details =
    let
        list =
            List.filter (\p -> p.key == name) details
    in
    case List.head list of
        Just subcondition ->
            subcondition.value

        _ ->
            ""


clear_model model =
    let
        form =
            model.form

        init_form =
            initial_form
    in
    { model
        | view = ViewUserableOptions
        , prev_action = Nothing
        , criteria = []
        , form =
            { form
                | name = ""
                , description = ""
                , and_or = ""
                , userable = init_form.userable
                , unregistered_users = init_form.unregistered_users
                , members = init_form.members
                , date_low = Nothing
                , date_high = Nothing
            }
        , mikol =
            { id = 0
            , and_or = ""
            , criteria = ""
            , description = ""
            , name = ""
            , resource_path = ""
            }
    }


show_submit_form model =
    { model
        | view = ViewSubmitForm
    }


submit model =
    let
        model_ =
            set_model_default_values model

        errors =
            build_submit_errors model_
    in
    case List.length errors of
        0 ->
            post_request model_

        _ ->
            let
                form =
                    model_.form

                updated_form =
                    { model_ | form = { form | errors = errors } }

                name =
                    model.form.name |> String.trim
            in
            case name of
                "" ->
                    ( updated_form, focus_on "ta_name" )

                _ ->
                    ( updated_form, focus_on "ta_description" )


focus_on dom_id =
    Task.attempt (\_ -> NoOp) (Dom.focus dom_id)


set_model_default_values model =
    case List.length model.criteria of
        1 ->
            case model.form.and_or of
                "" ->
                    let
                        form =
                            model.form
                    in
                    { model | form = { form | and_or = "||" } }

                _ ->
                    model

        _ ->
            model


post_request model =
    case model.flags.ta_id of
        "" ->
            post_ta_template_request model

        _ ->
            patch_ta_template_request model


build_submit_errors model =
    let
        name =
            model.form.name |> String.trim

        description =
            model.form.description |> String.trim

        and_or =
            model.form.and_or

        name_error =
            case name of
                "" ->
                    [ "Name must not be empty" ]

                _ ->
                    []

        description_error =
            case description of
                "" ->
                    [ "Description must not be empty" ]

                _ ->
                    []

        and_or_error =
            case and_or of
                "" ->
                    [ "Either ANY or ALL must be selected" ]

                _ ->
                    []
    in
    name_error ++ description_error ++ and_or_error


post_ta_template_request model =
    let
        body =
            post_ta_template_request_body model
                |> Encode.object
                |> Http.jsonBody

        expect =
            Rails.expectJson HandlePostTaTemplateResponse post_ta_template_decoder
    in
    ( model
    , Rails.post
        { url = model.flags.ta_templates_url
        , body = body
        , expect = expect
        }
    )


patch_ta_template_request model =
    let
        body =
            post_ta_template_request_body model
                |> Encode.object
                |> Http.jsonBody

        expect =
            Rails.expectJson HandlePatchTaTemplateResponse post_ta_template_decoder
    in
    ( model
    , Rails.patch
        { url = model.flags.ta_templates_url ++ "/" ++ model.flags.ta_id
        , body = body
        , expect = expect
        }
    )


post_ta_template_request_body model =
    let
        criteria =
            List.map encode_condition model.criteria
    in
    [ ( "id", Encode.string model.flags.ta_id )
    , ( "and_or", Encode.string model.form.and_or )
    , ( "criteria", Encode.list Encode.object criteria )
    , ( "description", Encode.string model.form.description )
    , ( "name", Encode.string model.form.name )
    ]


encode_condition condition =
    let
        value =
            List.map encode_subcondition condition.details
    in
    [ ( "user_type", Encode.string condition.user_type )
    , ( "name", Encode.string condition.name )
    , ( "details", Encode.list Encode.object value )
    ]


encode_subcondition subcondition =
    [ ( "key", Encode.string subcondition.key )
    , ( "value", Encode.string subcondition.value )
    ]


post_ta_template_decoder =
    succeed Mikol
        |> required "id" int
        |> required "and_or" string
        |> required "criteria" string
        |> required "description" string
        |> required "name" string
        |> required "resource_path" string


update_model msg model =
    let
        form =
            model.form

        members =
            form.members
    in
    case msg of
        SelectUserable ->
            let
                init_form =
                    initial_form
            in
            { model
                | view = ViewUserableOptions
                , form =
                    { form
                        | userable = NilUserable
                        , unregistered_users = init_form.unregistered_users
                        , members = init_form.members
                    }
            }
                |> clear_errors

        CancelAndRedirectToUnregisteredUsers ->
            cancel_and_redirect_to_unregistered_users model
                |> clear_errors

        SelectUnregisteredUsers ->
            cancel_and_redirect_to_unregistered_users model
                |> clear_errors

        CancelAndRedirectToMembers ->
            cancel_and_redirect_to_members model
                |> clear_errors

        SelectMembers ->
            cancel_and_redirect_to_members model
                |> clear_errors

        AddConditionAndRedirectToUnregisteredUsers ->
            model
                |> add_condition_and_redirect_to_unregistered_users
                |> set_criteria_updated True
                |> if_no_error clear_edit_idx

        AddConditionAndRedirectToMembers ->
            model
                |> add_condition_and_redirect_to_members
                |> set_criteria_updated True
                |> if_no_error clear_edit_idx

        RemoveCondition strIndex ->
            model
                |> remove_condition strIndex
                |> clear_edit_idx

        SelectRegistrationDateRange ->
            model
                |> select_form ViewRegistrationDateRangeForm

        SelectNotActiveForAtLeastXDays ->
            model
                |> select_form ViewNotActiveForAtLeastXDaysForm

        SelectActiveInTheLastXDays ->
            model
                |> select_form ViewActiveInTheLastXDaysForm

        SelectPaidPurchaseActivityInXDays ->
            { model | view = ViewMemberOptions }

        SelectHasNotPurchasedInXDays ->
            model
                |> select_form ViewHasNotPurchasedInXDaysForm

        SelectHasPurchasedInXDays ->
            model
                |> select_form ViewHasPurchasedInXDaysForm

        SelectPromotionUsage ->
            model
                |> select_form ViewPromotionUsageForm

        SelectHasNotUsedPromotion ->
            model
                |> select_form ViewHasNotUsedPromotionForm

        SelectHasUsedPromotion ->
            model
                |> select_form ViewHasUsedPromotionForm

        SelectVoucherUsage ->
            model
                |> select_form ViewVoucherUsageForm

        SelectHasVoucher ->
            model
                |> select_form ViewHasVoucher

        SelectHasVoucherButNeverUsed ->
            model
                |> select_form ViewHasVoucherButNeverUsedForm

        SelectHasUsedTheVoucher ->
            model
                |> select_form ViewHasUsedTheVoucherForm

        SelectDoesNotHaveTheVoucher ->
            model
                |> select_form ViewDoesNotHaveTheVoucherForm

        SelectPaidPurchaseCount ->
            model
                |> select_form ViewPaidPurchaseCountForm

        SelectBasketValueRange ->
            model
                |> select_form ViewBasketValueRangeForm

        SelectBasketQuantityRange ->
            model
                |> select_form ViewBasketQuantityRangeForm

        SelectHasNotUsedPromotionId strId ->
            let
                updated_members =
                    { members | has_not_used_promotion_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectHasUsedPromotionId strId ->
            let
                updated_members =
                    { members | has_used_promotion_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectHasVoucherId strId ->
            let
                updated_members =
                    { members | has_voucher_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectHasVoucherButNeverUsedId strId ->
            let
                updated_members =
                    { members | has_voucher_but_never_used_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectHasUsedTheVoucherId strId ->
            let
                updated_members =
                    { members | has_used_the_voucher_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectDoesNotHaveTheVoucherId strId ->
            let
                updated_members =
                    { members | does_not_have_the_voucher_id = strId }
            in
            { model | form = { form | members = updated_members } }

        SelectPaidPurchaseCountComparisonOperator str ->
            let
                updated_members =
                    { members | paid_purchase_count_comparison_operator = str }
            in
            { model | form = { form | members = updated_members } }

        SelectBasketValueComparisonOperator str ->
            let
                updated_members =
                    { members | basket_value_comparison_operator = str }
            in
            { model | form = { form | members = updated_members } }

        SelectBasketValueAggregation str ->
            let
                updated_members =
                    { members | basket_value_aggregation = str }
            in
            { model | form = { form | members = updated_members } }

        SelectBasketQuantityComparisonOperator str ->
            let
                updated_members =
                    { members | basket_quantity_comparison_operator = str }
            in
            { model | form = { form | members = updated_members } }

        SelectDateLow subMsg ->
            update_date_low model subMsg

        SelectDateHigh subMsg ->
            update_date_high model subMsg

        _ ->
            model
                |> update_form_data msg


select_form view_form model =
    let
        form =
            model.form
    in
    case model.form.edit_idx of
        Just edit_idx ->
            if edit_idx == "-1" then
                -- NEW ENTRY FORM
                { model | view = view_form }
                    |> initialize_form

            else
                -- EDIT FORM
                { model | view = view_form }

        Nothing ->
            -- NEW ENTRY FORM
            { model
                | view = view_form
                , form = { form | edit_idx = Nothing }
            }
                |> initialize_form


set_criteria_updated bool model =
    { model | criteria_updated = bool }


add_condition_and_redirect_to_unregistered_users model =
    model
        |> add_condition
        |> if_no_error (set_view ViewUnregisteredUserOptions)


add_condition_and_redirect_to_members model =
    model
        |> add_condition
        |> if_no_error (set_view ViewMemberOptions)


add_condition model =
    model
        |> clear_errors
        |> if_no_error update_criteria
        |> if_no_error initialize_form


set_view view model =
    { model | view = view }


initialize_form model =
    let
        { unregistered_users, members } =
            initial_form

        { form } =
            model
    in
    { model
        | form =
            { form
                | unregistered_users = unregistered_users
                , members = members
                , date_high = Nothing
                , date_low = Nothing
                , errors = []
            }
    }


update_criteria model =
    model
        |> clear_errors
        |> Validate.validate_form
        |> if_no_error remove_item_at_edit_idx
        |> if_no_error update_fx


update_fx model =
    case model.view of
        ViewRegistrationDateRangeForm ->
            update_criteria_registration_date_range model

        ViewNotActiveForAtLeastXDaysForm ->
            update_criteria_not_active_for_at_least_x_days model

        ViewActiveInTheLastXDaysForm ->
            update_criteria_active_in_the_last_x_days model

        ViewHasNotPurchasedInXDaysForm ->
            update_criteria_has_not_purchased_in_x_days model

        ViewHasPurchasedInXDaysForm ->
            update_criteria_has_purchased_in_x_days model

        ViewHasNotUsedPromotionForm ->
            update_criteria_has_not_used_promotion_id model

        ViewHasUsedPromotionForm ->
            update_criteria_has_used_promotion_id model

        ViewHasVoucher ->
            update_criteria_has_voucher model

        ViewHasVoucherButNeverUsedForm ->
            update_criteria_has_voucher_but_never_used_id model

        ViewHasUsedTheVoucherForm ->
            update_criteria_has_used_the_voucher_id model

        ViewDoesNotHaveTheVoucherForm ->
            update_criteria_does_not_have_the_voucher_id model

        ViewPaidPurchaseCountForm ->
            update_criteria_paid_purchase_count model

        ViewBasketValueRangeForm ->
            update_criteria_basket_value model

        ViewBasketQuantityRangeForm ->
            update_criteria_basket_quantity model

        _ ->
            model


if_no_error fx model =
    case model.form.errors of
        [] ->
            fx model

        _ ->
            model


remove_item_at_edit_idx model =
    case model.form.edit_idx of
        Just edit_idx_str ->
            case String.toInt edit_idx_str of
                Just edit_idx ->
                    { model
                        | criteria =
                            List.Extra.removeAt edit_idx model.criteria
                    }

                _ ->
                    model

        _ ->
            model


update_criteria_not_active_for_at_least_x_days model =
    let
        criteria =
            model.criteria

        days =
            case model.form.userable of
                UnregisteredUsers ->
                    model.form.unregistered_users.not_active_for_at_least_x_days |> String.trim

                _ ->
                    model.form.members.not_active_for_at_least_x_days |> String.trim
    in
    case days of
        "" ->
            model

        _ ->
            let
                updated_criteria =
                    { user_type = user_type_str model
                    , name = "not_active_for_at_least_x_days"
                    , details =
                        [ { key = "not_active_for_at_least_x_days"
                          , value = days
                          }
                        ]
                    }
                        :: criteria
            in
            { model | criteria = updated_criteria }


update_criteria_active_in_the_last_x_days model =
    let
        criteria =
            model.criteria

        days =
            case model.form.userable of
                UnregisteredUsers ->
                    model.form.unregistered_users.active_in_the_last_x_days |> String.trim

                _ ->
                    model.form.members.active_in_the_last_x_days |> String.trim
    in
    case days of
        "" ->
            model

        _ ->
            let
                updated_criteria =
                    { user_type = user_type_str model
                    , name = "active_in_the_last_x_days"
                    , details =
                        [ { key = "active_in_the_last_x_days"
                          , value = days
                          }
                        ]
                    }
                        :: criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_not_purchased_in_x_days model =
    let
        criteria =
            model.criteria

        days =
            model.form.members.has_not_purchased_in_x_days |> String.trim
    in
    case days of
        "" ->
            model

        _ ->
            let
                updated_criteria =
                    { user_type = user_type_str model
                    , name = "has_not_purchased_in_x_days"
                    , details =
                        [ { key = "has_not_purchased_in_x_days"
                          , value = days
                          }
                        ]
                    }
                        :: criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_purchased_in_x_days model =
    let
        criteria =
            model.criteria

        days =
            model.form.members.has_purchased_in_x_days |> String.trim
    in
    case days of
        "" ->
            model

        _ ->
            let
                updated_criteria =
                    { user_type = user_type_str model
                    , name = "has_purchased_in_x_days"
                    , details =
                        [ { key = "has_purchased_in_x_days"
                          , value = days
                          }
                        ]
                    }
                        :: criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_not_used_promotion_id model =
    let
        val =
            model.form.members.has_not_used_promotion_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "has_not_used_promotion_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "has_not_used_promotion_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "has_not_used_promotion_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "has_not_used_promotion_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_used_promotion_id model =
    let
        val =
            model.form.members.has_used_promotion_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "has_used_promotion_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "has_used_promotion_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "has_used_promotion_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "has_used_promotion_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_voucher model =
    let
        val =
            model.form.members.has_voucher_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "has_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "has_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "has_voucher_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "has_voucher_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_voucher_but_never_used_id model =
    let
        val =
            model.form.members.has_voucher_but_never_used_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "has_voucher_but_never_used_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "has_voucher_but_never_used_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "has_voucher_but_never_used_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "has_voucher_but_never_used_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_has_used_the_voucher_id model =
    let
        val =
            model.form.members.has_used_the_voucher_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "has_used_the_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "has_used_the_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "has_used_the_voucher_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "has_used_the_voucher_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_does_not_have_the_voucher_id model =
    let
        val =
            model.form.members.does_not_have_the_voucher_id |> String.trim
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "does_not_have_the_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "does_not_have_the_voucher_id"
                              , value = val
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "does_not_have_the_voucher_id"
                              , value = val
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "does_not_have_the_voucher_id", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_paid_purchase_count model =
    let
        val =
            model.form.members.paid_purchase_count |> String.trim

        comparison_operator =
            model.form.members.paid_purchase_count_comparison_operator
    in
    case val of
        "" ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "paid_purchase_count"
                              , value = val
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "paid_purchase_count"
                              , value = val
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "paid_purchase_count"
                              , value = val
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "paid_purchase_count", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_basket_value model =
    let
        basket_value =
            model.form.members.basket_value |> String.trim

        comparison_operator =
            model.form.members.basket_value_comparison_operator

        aggregation =
            model.form.members.basket_value_aggregation
    in
    case ( basket_value, comparison_operator, aggregation ) of
        ( "", _, _ ) ->
            model

        ( _, "", _ ) ->
            model

        ( _, _, "" ) ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "basket_value", value = basket_value }
                            , { key = "comparison_operator", value = comparison_operator }
                            , { key = "aggregation", value = aggregation }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "basket_value", value = basket_value }
                            , { key = "comparison_operator", value = comparison_operator }
                            , { key = "aggregation", value = aggregation }
                            ]

                        ( _, _ ) ->
                            [ { key = "basket_value", value = basket_value }
                            , { key = "comparison_operator", value = comparison_operator }
                            , { key = "aggregation", value = aggregation }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "basket_value", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


update_criteria_registration_date_range model =
    let
        value =
            case ( model.form.date_low, model.form.date_high ) of
                ( Nothing, _ ) ->
                    []

                ( _, Nothing ) ->
                    []

                ( _, _ ) ->
                    [ { key = "inclusive_dates"
                      , value =
                            date_format model.form.date_low
                                ++ " - "
                                ++ date_format model.form.date_high
                      }
                    ]

        updated_criteria =
            { user_type = user_type_str model, name = "registration_date", details = value } :: model.criteria
    in
    { model | criteria = updated_criteria }


update_criteria_basket_quantity model =
    let
        basket_quantity =
            model.form.members.basket_quantity |> String.trim

        comparison_operator =
            model.form.members.basket_quantity_comparison_operator
    in
    case ( basket_quantity, comparison_operator ) of
        ( "", _ ) ->
            model

        ( _, "" ) ->
            model

        _ ->
            let
                value =
                    case ( model.form.date_low, model.form.date_high ) of
                        ( Nothing, _ ) ->
                            [ { key = "basket_quantity"
                              , value = basket_quantity
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            ]

                        ( _, Nothing ) ->
                            [ { key = "basket_quantity"
                              , value = basket_quantity
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            ]

                        ( _, _ ) ->
                            [ { key = "basket_quantity"
                              , value = basket_quantity
                              }
                            , { key = "comparison_operator"
                              , value = comparison_operator
                              }
                            , { key = "inclusive_dates"
                              , value =
                                    date_format model.form.date_low
                                        ++ " - "
                                        ++ date_format model.form.date_high
                              }
                            ]

                updated_criteria =
                    { user_type = user_type_str model, name = "basket_quantity", details = value } :: model.criteria
            in
            { model | criteria = updated_criteria }


date_format maybe_date =
    case maybe_date of
        Just date ->
            Date.format "yyyy-MM-dd" date

        Nothing ->
            "No date."


user_type_str model =
    case model.form.userable of
        UnregisteredUsers ->
            "Unregistered Users"

        Members ->
            "Members"

        _ ->
            ""


remove_condition strIndex model =
    let
        criteria =
            case String.toInt strIndex of
                Just idx ->
                    List.Extra.removeAt idx model.criteria

                _ ->
                    model.criteria
    in
    case List.length criteria of
        0 ->
            { model | criteria = [], view = ViewUserableOptions }
                |> clear_errors

        _ ->
            { model | criteria = criteria }


clear_edit_idx model =
    case model.form.edit_idx of
        Just edit_idx ->
            let
                form =
                    model.form
            in
            { model
                | form = { form | edit_idx = Nothing }
            }

        _ ->
            model


cancel_and_redirect_to_unregistered_users model =
    let
        init_form =
            initial_form

        form =
            model.form
    in
    { model
        | view = ViewUnregisteredUserOptions
        , form =
            { form
                | userable = UnregisteredUsers
                , unregistered_users = init_form.unregistered_users
                , members = init_form.members
                , date_high = Nothing
                , date_low = Nothing
                , edit_idx = Nothing
            }
    }


cancel_and_redirect_to_members model =
    let
        init_form =
            initial_form

        form =
            model.form
    in
    { model
        | view = ViewMemberOptions
        , form =
            { form
                | userable = Members
                , unregistered_users = init_form.unregistered_users
                , members = init_form.members
                , date_high = Nothing
                , date_low = Nothing
                , edit_idx = Nothing
            }
    }


unpack_int default value =
    Maybe.withDefault default <| String.toInt value


update_date_low model subMsg =
    let
        form =
            model.form

        members =
            form.members

        updates =
            update_date_field model datepicker_low_settings .date_low .datepicker_low subMsg
    in
    { model
        | form =
            { form
                | date_low = updates.date
                , datepicker_low = updates.datepicker
            }
    }


update_date_high model subMsg =
    let
        form =
            model.form

        members =
            form.members

        updates =
            update_date_field model datepicker_high_settings .date_high .datepicker_high subMsg
    in
    { model
        | form =
            { form
                | date_high = updates.date
                , datepicker_high = updates.datepicker
            }
    }


update_date_field model settings date_key datepicker_key subMsg =
    let
        form =
            model.form

        ( newDatePicker, dateEvent ) =
            DatePicker.update settings subMsg (datepicker_key form)

        newDate =
            case dateEvent of
                Picked changedDate ->
                    Just changedDate

                _ ->
                    date_key form
    in
    { date = newDate
    , datepicker = newDatePicker
    }


update_form_data msg model =
    case model.view of
        ViewSubmitForm ->
            let
                form =
                    model.form
            in
            case msg of
                UpdateTemplateName strValue ->
                    { model
                        | form = { form | name = strValue }
                    }

                UpdateTemplateDescription strValue ->
                    { model
                        | form = { form | description = strValue }
                    }

                SelectAndOrCondition strValue ->
                    { model
                        | form = { form | and_or = strValue }
                    }

                _ ->
                    model

        _ ->
            case model.form.userable of
                UnregisteredUsers ->
                    update_unregistered_users_form msg model

                _ ->
                    update_members_form msg model


update_unregistered_users_form msg model =
    let
        form =
            model.form

        unregistered_users =
            form.unregistered_users
    in
    case msg of
        UpdateNotActiveForAtLeastXDays strValue ->
            { model
                | form =
                    { form
                        | unregistered_users = { unregistered_users | not_active_for_at_least_x_days = strValue }
                    }
            }

        UpdateActiveInTheLastXDays strValue ->
            { model
                | form =
                    { form
                        | unregistered_users = { unregistered_users | active_in_the_last_x_days = strValue }
                    }
            }

        _ ->
            model


update_members_form msg model =
    let
        form =
            model.form

        members =
            form.members
    in
    case msg of
        UpdateNotActiveForAtLeastXDays strValue ->
            { model
                | form =
                    { form
                        | members = { members | not_active_for_at_least_x_days = strValue }
                    }
            }

        UpdateActiveInTheLastXDays strValue ->
            { model
                | form =
                    { form
                        | members = { members | active_in_the_last_x_days = strValue }
                    }
            }

        UpdateHasNotPurchasedInXDays strValue ->
            { model
                | form =
                    { form
                        | members = { members | has_not_purchased_in_x_days = strValue }
                    }
            }

        UpdateHasPurchasedInXDays strValue ->
            { model
                | form =
                    { form
                        | members = { members | has_purchased_in_x_days = strValue }
                    }
            }

        UpdatePaidPurchaseCount strValue ->
            { model
                | form =
                    { form
                        | members = { members | paid_purchase_count = strValue }
                    }
            }

        UpdateBasketValue strValue ->
            { model
                | form =
                    { form
                        | members = { members | basket_value = strValue }
                    }
            }

        UpdateBasketQuantity strValue ->
            { model
                | form =
                    { form
                        | members = { members | basket_quantity = strValue }
                    }
            }

        _ ->
            model


handle_post_ta_template_response model result =
    let
        return_value =
            case result of
                Ok mikol ->
                    { model | mikol = mikol }

                _ ->
                    model

        resource_path =
            return_value.mikol.resource_path
    in
    ( clear_model model, openWindow resource_path )


handle_patch_ta_template_response model result =
    let
        return_value =
            case result of
                Ok mikol ->
                    { model | mikol = mikol }

                _ ->
                    model

        resource_path =
            model.admin_ta_template_path
    in
    ( clear_model model, openWindow resource_path )


handle_get_ta_template_response model result =
    let
        ta_template =
            case result of
                Ok ta_template_ ->
                    ta_template_

                _ ->
                    { id = 0
                    , name = ""
                    , description = ""
                    , criteria = ""
                    , and_or = ""
                    , resource_path = ""
                    }

        criteria =
            case decodeString (list condition_decoder) ta_template.criteria of
                Ok criteria_ ->
                    criteria_

                _ ->
                    []

        cls_model =
            clear_model model

        form =
            cls_model.form
    in
    ( { cls_model
        | criteria = criteria
        , admin_ta_template_path = ta_template.resource_path
        , form =
            { form
                | name = ta_template.name
                , description = ta_template.description
                , and_or = ta_template.and_or
            }
      }
    , Cmd.none
    )
