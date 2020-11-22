module Model exposing (Condition, Criteria, Flags, Mikol, Model, Msg(..), Subcondition, TaTemplate, Userable(..), ViewTypes(..), condition_decoder, datepicker_high_settings, datepicker_low_settings, edit_idx_int, get_ta_template_decoder, get_ta_template_request, init, initial_form, initial_model, sub_condition_decoder, ta_template_decoder, true_edit_idx)

import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (..)
import Http
import Json.Decode exposing (Decoder, at, bool, decodeString, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Rails


type Msg
    = SelectUserable
    | SelectUnregisteredUsers
    | SelectMembers
    | SelectBrowsingActivityForAtLeastXDays
    | SelectRegistrationDateRange
    | SelectNotActiveForAtLeastXDays
    | SelectActiveInTheLastXDays
    | SelectPaidPurchaseActivityInXDays
    | SelectHasNotPurchasedInXDays
    | SelectHasPurchasedInXDays
    | SelectPromotionUsage
    | SelectHasNotUsedPromotion
    | SelectHasUsedPromotion
    | SelectVoucherUsage
    | SelectHasVoucher
    | SelectHasVoucherButNeverUsed
    | SelectHasUsedTheVoucher
    | SelectDoesNotHaveTheVoucher
    | SelectPaidPurchaseCount
    | SelectBasketValueRange
    | SelectBasketQuantityRange
    | UpdateNotActiveForAtLeastXDays String
    | UpdateActiveInTheLastXDays String
    | UpdateHasNotPurchasedInXDays String
    | UpdateHasPurchasedInXDays String
    | UpdatePaidPurchaseCount String
    | SelectPaidPurchaseCountComparisonOperator String
    | SelectHasNotUsedPromotionId String
    | SelectHasUsedPromotionId String
    | SelectHasVoucherId String
    | SelectHasVoucherButNeverUsedId String
    | SelectHasUsedTheVoucherId String
    | SelectDoesNotHaveTheVoucherId String
    | SelectDateLow DatePicker.Msg
    | SelectDateHigh DatePicker.Msg
    | UpdateBasketValue String
    | SelectBasketValueComparisonOperator String
    | SelectBasketValueAggregation String
    | UpdateBasketQuantity String
    | SelectBasketQuantityComparisonOperator String
    | CancelAndRedirectToUnregisteredUsers
    | CancelAndRedirectToMembers
    | AddConditionAndRedirectToUnregisteredUsers
    | AddConditionAndRedirectToMembers
    | SelectCondition String
    | EditCondition String
    | RemoveCondition String
    | Proceed
    | UpdateTemplateName String
    | UpdateTemplateDescription String
    | SelectAndOrCondition String
    | Submit
    | HandlePostTaTemplateResponse (Result Http.Error Mikol)
    | HandlePatchTaTemplateResponse (Result Http.Error Mikol)
    | HandleGetTaTemplateResponse (Result Http.Error TaTemplate)
    | EnterWasPressed
    | EscWasPressed
    | ClearSelectCondition
    | EditSelectedCondition
    | UpSelectCondition
    | DownSelectCondition
    | NoOp


type ViewTypes
    = ViewUserableOptions
    | ViewUnregisteredUserOptions
    | ViewMemberOptions
    | ViewRegistrationDateRangeForm
    | ViewNotActiveForAtLeastXDaysForm
    | ViewActiveInTheLastXDaysForm
    | ViewPaidPurchaseActivityInXDaysForm
    | ViewHasNotPurchasedInXDaysForm
    | ViewHasPurchasedInXDaysForm
    | ViewPromotionUsageForm
    | ViewHasNotUsedPromotionForm
    | ViewHasUsedPromotionForm
    | ViewVoucherUsageForm
    | ViewHasVoucher
    | ViewHasVoucherButNeverUsedForm
    | ViewHasUsedTheVoucherForm
    | ViewDoesNotHaveTheVoucherForm
    | ViewPaidPurchaseCountForm
    | ViewBasketValueRangeForm
    | ViewBasketQuantityRangeForm
    | ViewSubmitForm


type Userable
    = UnregisteredUsers
    | Members
    | NilUserable


type alias Mikol =
    { id : Int
    , and_or : String
    , criteria : String
    , description : String
    , name : String
    , resource_path : String
    }


type alias Flags =
    { vouchers : List ( Int, String )
    , promotions : List ( Int, String )
    , ta_templates_url : String
    , ta_id : String
    }


type alias Model =
    { flags : Flags
    , admin_ta_template_path : String
    , view : ViewTypes
    , prev_action : Maybe Msg
    , criteria : Criteria
    , criteria_updated : Bool
    , form :
        { name : String
        , description : String
        , and_or : String
        , userable : Userable
        , unregistered_users :
            { not_active_for_at_least_x_days : String
            , active_in_the_last_x_days : String
            }
        , members :
            { not_active_for_at_least_x_days : String
            , active_in_the_last_x_days : String
            , has_not_purchased_in_x_days : String
            , has_purchased_in_x_days : String
            , has_not_used_promotion_id : String
            , has_used_promotion_id : String
            , has_voucher_id : String
            , has_voucher_but_never_used_id : String
            , has_used_the_voucher_id : String
            , does_not_have_the_voucher_id : String
            , paid_purchase_count : String
            , paid_purchase_count_comparison_operator : String
            , basket_value : String
            , basket_value_comparison_operator : String
            , basket_value_aggregation : String
            , basket_quantity : String
            , basket_quantity_comparison_operator : String
            , basket_quantity_aggregation : String
            }
        , date_low : Maybe Date
        , datepicker_low : DatePicker.DatePicker
        , date_high : Maybe Date
        , datepicker_high : DatePicker.DatePicker
        , errors : List String
        , edit_idx : Maybe String
        }
    , mikol : Mikol
    }


init : Flags -> ( Model, Cmd Msg )
init { vouchers, promotions, ta_templates_url, ta_id } =
    let
        ( datepicker_low_, datepicker_low_fx_ ) =
            DatePicker.init

        ( datepicker_high_, datepicker_high_fx_ ) =
            DatePicker.init

        datepicker_commands =
            [ Cmd.map SelectDateLow datepicker_low_fx_
            , Cmd.map SelectDateHigh datepicker_high_fx_
            ]

        commands =
            case ta_id of
                "" ->
                    datepicker_commands

                _ ->
                    get_ta_template_request ta_templates_url ta_id :: datepicker_commands
    in
    ( initial_model
        vouchers
        promotions
        ta_templates_url
        ta_id
        datepicker_low_
        datepicker_high_
    , Cmd.batch commands
    )


get_ta_template_request ta_templates_url ta_id =
    let
        expect =
            Rails.expectJson HandleGetTaTemplateResponse ta_template_decoder
    in
    Rails.get
        { url = ta_templates_url ++ "/" ++ ta_id
        , expect = expect
        }


get_ta_template_decoder =
    succeed Mikol
        |> required "id" int
        |> required "and_or" string
        |> required "criteria" string
        |> required "description" string
        |> required "name" string
        |> required "resource_path" string


type alias TaTemplate =
    { id : Int
    , name : String
    , description : String
    , criteria : String
    , and_or : String
    , resource_path : String
    }


type alias Criteria =
    List Condition


type alias Condition =
    { user_type : String
    , name : String
    , details : List Subcondition
    }


type alias Subcondition =
    { key : String, value : String }


ta_template_decoder =
    Json.Decode.map6 TaTemplate
        (at [ "id" ] int)
        (at [ "name" ] string)
        (at [ "description" ] string)
        (at [ "criteria" ] string)
        (at [ "and_or" ] string)
        (at [ "resource_path" ] string)


condition_decoder =
    Json.Decode.map3 Condition
        (at [ "user_type" ] string)
        (at [ "name" ] string)
        (at [ "details" ] (list sub_condition_decoder))


sub_condition_decoder =
    map2 Subcondition
        (at [ "key" ] string)
        (at [ "value" ] string)


initial_model vouchers promotions ta_templates_url ta_id datepicker_low_ datepicker_high_ =
    let
        init_form =
            initial_form
    in
    { flags =
        { vouchers = vouchers
        , promotions = promotions
        , ta_templates_url = ta_templates_url
        , ta_id = ta_id
        }
    , admin_ta_template_path = ""
    , view = ViewUserableOptions
    , prev_action = Nothing
    , criteria = []
    , criteria_updated = False
    , form =
        { name = ""
        , description = ""
        , and_or = ""
        , userable = init_form.userable
        , unregistered_users = init_form.unregistered_users
        , members = init_form.members
        , date_low = Nothing
        , datepicker_low = datepicker_low_
        , date_high = Nothing
        , datepicker_high = datepicker_high_
        , errors = []
        , edit_idx = Nothing
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


initial_form =
    { userable = NilUserable
    , unregistered_users =
        { not_active_for_at_least_x_days = ""
        , active_in_the_last_x_days = ""
        }
    , members =
        { not_active_for_at_least_x_days = ""
        , active_in_the_last_x_days = ""
        , has_not_purchased_in_x_days = ""
        , has_purchased_in_x_days = ""
        , has_not_used_promotion_id = ""
        , has_used_promotion_id = ""
        , has_voucher_id = ""
        , has_voucher_but_never_used_id = ""
        , has_used_the_voucher_id = ""
        , does_not_have_the_voucher_id = ""
        , paid_purchase_count = ""
        , paid_purchase_count_comparison_operator = ""
        , basket_value = ""
        , basket_value_comparison_operator = ""
        , basket_value_aggregation = ""
        , basket_quantity = ""
        , basket_quantity_comparison_operator = ""
        , basket_quantity_aggregation = ""
        }
    }


datepicker_low_settings =
    { defaultSettings | placeholder = "Start date" }


datepicker_high_settings =
    { defaultSettings | placeholder = "End date" }


true_edit_idx : Model -> Maybe Int
true_edit_idx model =
    case edit_idx_int model of
        Just idx ->
            if idx >= 1000 then
                Just (remainderBy 1000 idx)

            else
                Just idx

        _ ->
            Nothing


edit_idx_int : Model -> Maybe Int
edit_idx_int model =
    case model.form.edit_idx of
        Just edit_idx ->
            String.toInt edit_idx

        _ ->
            Nothing
