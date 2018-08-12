
#' A big wrapper of get user insurance discount coef.
#'
#' This function wrap all the functions to get the raw risk factors speicified
#' score, daymileage, discount coef. Before use this function, pleage make sure
#' that your data and cofiguration are correct.
#'
#' @param df Raw risk factors data.frame/tibble with the correct col names.
#' @param risk_names Correct col names to calculate.
#' @param score_config_list A List contains all riskFactors value and score
#'   index, read from .RDS file.
#' @param rf_weight Correct col names of risk factors weight, sum to 1.
#' @param discount_index Specified score and coef.
#'
#' @return a \code{\link[tibble:as_tibble]{tibble}} / data.frame
#' @author ZhaoWei
#' @export
#'
#' @examples
#' \dontrun{
#' cal_user_coef(pj_rf, "mielage", score_config_list, rf_weight, discount_index)
#'
#' risk_names <- c("acc_count_phk","act_radius","dec_count_phk","high_curv_tr","holiday_tr",
#'              "interstate_r","lane_change_phk","late_night_tr","long_tr","main_act_prov" ,
#'              "mileage","speeding_lvl", "speeding_phk", "trip_dis_e", "turn_count_phk")
#' cal_user_coef(pj_rf, risk_names, score_config_list, rf_weight, discount_index)
#' }
cal_user_coef <- function(df, risk_names, score_config_list,
                          rf_weight, discount_index) {
  day_mileage <- round(df$day_mileage, 2)

  if (length(risk_names) == 1) {
    tmp <- df$risk_names
    idx <- grep(risk_names, names(score_config_list))
    stopifnot(length(idx) == 2)
    # since one risk fator, the weight is 1.
    sum_score <- round(get_score_vec(
      tmp, score_config_list[[idx[1]]],
      score_config_list[[idx[2]]]
    ),
    digits = 2
    )
  } else {
    rf_df <- df[, risk_names]

    rf_score <- rf2score(rf_df, score_config_list)
    sum_score <- round(sum_rf_score(rf_score, rf_weight)$score,
      digits = 2
    )
  }

  discount_coef <- round(get_score_vec(
    sum_score, discount_index$score,
    discount_index$coef
  ),
  digits = 4
  )

  discount <- round(1 - discount_coef, digits = 6)
  n <- length(discount)
  delta <- round(c(
    discount[1],
    discount[-1] - discount[1:(n - 1)]
  ))

  result <- data.frame(
    score = sum_score,
    day_mileage = day_mileage,
    discount_coef = discount_coef,
    discount = discount,
    delta = delta
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  return(result)
}
