module Validate exposing (validate_form, validate_inclusive_dates, validate_integer, validate_required_inclusive_dates)

import Date exposing (Date, day, month, weekday, year)
import Model exposing (..)


validate_form model =
    let
        not_active_for_at_least_x_days =
            case model.form.userable of
                UnregisteredUsers ->
                    model.form.unregistered_users.not_active_for_at_least_x_days |> String.trim

                _ ->
                    model.form.members.not_active_for_at_least_x_days |> String.trim

        active_in_the_last_x_days =
            case model.form.userable of
                UnregisteredUsers ->
                    model.form.unregistered_users.active_in_the_last_x_days |> String.trim

                _ ->
                    model.form.members.active_in_the_last_x_days |> String.trim

        has_not_purchased_in_x_days =
            model.form.members.has_not_purchased_in_x_days |> String.trim

        has_purchased_in_x_days =
            model.form.members.has_purchased_in_x_days |> String.trim

        has_not_used_promotion_id =
            model.form.members.has_not_used_promotion_id |> String.trim

        has_used_promotion_id =
            model.form.members.has_used_promotion_id |> String.trim

        has_voucher_id =
            model.form.members.has_voucher_id |> String.trim

        has_voucher_but_never_used_id =
            model.form.members.has_voucher_but_never_used_id |> String.trim

        has_used_the_voucher_id =
            model.form.members.has_used_the_voucher_id |> String.trim

        does_not_have_the_voucher_id =
            model.form.members.does_not_have_the_voucher_id |> String.trim

        paid_purchase_count =
            model.form.members.paid_purchase_count |> String.trim

        paid_purchase_count_comparison_operator =
            model.form.members.paid_purchase_count_comparison_operator |> String.trim

        basket_value =
            model.form.members.basket_value |> String.trim

        basket_value_comparison_operator =
            model.form.members.basket_value_comparison_operator |> String.trim

        basket_value_aggregation =
            model.form.members.basket_value_aggregation |> String.trim

        basket_quantity =
            model.form.members.basket_quantity |> String.trim

        basket_quantity_comparison_operator =
            model.form.members.basket_quantity_comparison_operator |> String.trim

        form =
            model.form
    in
    case model.view of
        ViewRegistrationDateRangeForm ->
            model
                |> validate_required_inclusive_dates

        ViewNotActiveForAtLeastXDaysForm ->
            model
                |> validate_integer { low_limit = 0, high_limit = 1000 } not_active_for_at_least_x_days "Number of days"

        ViewActiveInTheLastXDaysForm ->
            model
                |> validate_integer { low_limit = 1, high_limit = 1000 } active_in_the_last_x_days "Number of days"

        ViewHasNotPurchasedInXDaysForm ->
            model
                |> validate_integer { low_limit = 1, high_limit = 1000 } has_not_purchased_in_x_days "Number of days"

        ViewHasPurchasedInXDaysForm ->
            model
                |> validate_integer { low_limit = 1, high_limit = 1000 } has_purchased_in_x_days "Number of days"

        ViewHasNotUsedPromotionForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case has_not_used_promotion_id of
                            "" ->
                                { model_ | form = { form | errors = "Promotion must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewHasUsedPromotionForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case has_used_promotion_id of
                            "" ->
                                { model_ | form = { form | errors = "Promotion must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewHasVoucher ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case has_voucher_id of
                            "" ->
                                { model_ | form = { form | errors = "Voucher must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewHasVoucherButNeverUsedForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case has_voucher_but_never_used_id of
                            "" ->
                                { model_ | form = { form | errors = "Voucher must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewHasUsedTheVoucherForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case has_used_the_voucher_id of
                            "" ->
                                { model_ | form = { form | errors = "Voucher must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewDoesNotHaveTheVoucherForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        case does_not_have_the_voucher_id of
                            "" ->
                                { model_ | form = { form | errors = "Voucher must not be empty" :: model_.form.errors } }

                            _ ->
                                model_
                   )

        ViewPaidPurchaseCountForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        (case paid_purchase_count_comparison_operator of
                            "" ->
                                { model_
                                    | form =
                                        { form | errors = "Comparison operator must not be empty" :: model_.form.errors }
                                }

                            _ ->
                                model_
                        )
                            |> validate_integer { low_limit = 0, high_limit = 1000 } paid_purchase_count "Paid purchase count"
                   )

        ViewBasketValueRangeForm ->
            model
                |> validate_inclusive_dates
                |> validate_value basket_value_aggregation "Order aggregation must not be empty"
                |> validate_value basket_value_comparison_operator "Comparison operator must not be empty"
                |> validate_integer { low_limit = 1, high_limit = 1000 } basket_value "Basket value"

        ViewBasketQuantityRangeForm ->
            model
                |> validate_inclusive_dates
                |> (\model_ ->
                        (case basket_quantity_comparison_operator of
                            "" ->
                                { model_
                                    | form =
                                        { form | errors = "Comparison operator must not be empty" :: model_.form.errors }
                                }

                            _ ->
                                model_
                        )
                            |> validate_integer { low_limit = 1, high_limit = 100 } basket_quantity "Basket quantity"
                   )

        _ ->
            model


validate_value value error_message model =
    let
        form =
            model.form
    in
    case value of
        "" ->
            { model
                | form =
                    { form | errors = error_message :: model.form.errors }
            }

        _ ->
            model


validate_integer { low_limit, high_limit } value name model =
    let
        error_message =
            case value of
                "" ->
                    name ++ " must be a valid non-negative number"

                _ ->
                    case String.toInt value of
                        Just int_val ->
                            if int_val < low_limit then
                                name ++ " must not be lower than " ++ String.fromInt low_limit

                            else if int_val > high_limit then
                                name ++ " must not be higher than " ++ String.fromInt high_limit

                            else
                                ""

                        _ ->
                            name ++ " must be a valid non-negative number"
    in
    case error_message of
        "" ->
            model

        _ ->
            let
                form =
                    model.form
            in
            { model | form = { form | errors = error_message :: form.errors } }


validate_required_inclusive_dates model =
    let
        form =
            model.form
    in
    case ( model.form.date_low, model.form.date_high ) of
        ( Nothing, Nothing ) ->
            { model | form = { form | errors = "Start and end dates must not be empty" :: model.form.errors } }

        ( Nothing, _ ) ->
            { model | form = { form | errors = "Start date must not be empty" :: model.form.errors } }

        ( _, Nothing ) ->
            { model | form = { form | errors = "End date must not be empty" :: model.form.errors } }

        ( Just date_low, Just date_high ) ->
            case Date.compare date_low date_high of
                EQ ->
                    model

                LT ->
                    model

                _ ->
                    { model | form = { form | errors = "Start date must be earlier than end date" :: model.form.errors } }


validate_inclusive_dates model =
    let
        form =
            model.form
    in
    case ( model.form.date_low, model.form.date_high ) of
        ( Nothing, Nothing ) ->
            model

        ( Nothing, _ ) ->
            { model | form = { form | errors = "Start date must not be empty" :: model.form.errors } }

        ( _, Nothing ) ->
            { model | form = { form | errors = "End date must not be empty" :: model.form.errors } }

        ( Just date_low, Just date_high ) ->
            case Date.compare date_low date_high of
                EQ ->
                    model

                LT ->
                    model

                _ ->
                    { model | form = { form | errors = "Start date must be earlier than end date" :: model.form.errors } }
