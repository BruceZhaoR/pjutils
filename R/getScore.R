
#' Get the score of a risk factor according to the given config.
#'
#'
#' @author ZhaoWei
#' @param riskfactor Raw risk factor.
#' @param values Specified risk factors.
#' @param scores Specified score according to the value.
#'
#' @return A numeric score.
#' @export
#' @examples
#' values <- c(55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
#' scores <- c(1.0000, 0.9747, 0.8967, 0.8303, 0.7730, 0.7232, 0.6793, 0.6405, 0.6059, 0.5748)
#' get_score(57.5, values, scores) # 0.98735
#' get_score(97.5, values, scores) # 0.59035
#' get_score(77, values, scores) # 0.75308
#'
get_score <-function(riskfactor, values, scores){
  # valuse and scores lenght should be equal
  stopifnot(length(values) == length(scores))
  stopifnot(is.vector(values) && is.vector(scores, mode = "numeric"))

  if (is.numeric(riskfactor)) {

    if(riskfactor < values[1]){
      return(scores[1])
    }

    if(riskfactor > values[length(values)]){
      return(0.0);
    }

    if (length(values) < 10000) {
      idx <- binary_search_cpp(riskfactor, values) + 1
    } else {
      idx <- binary_search_r(riskfactor, values)
    }

    if (idx <= 1) {
      return(scores[1])
    }
    if(idx > length(scores)){
      return(scores[length(scores)])
    }

    delta <- (riskfactor - values[idx-1]) / (values[idx] - values[idx-1])

    return(scores[idx-1] + delta * (scores[idx] - scores[idx-1]))
  }
  if (is.character(riskfactor)) {
    if (riskfactor %in% values) {
      idx <- grep(riskfactor, values, fixed = TRUE)
    } else {
      warning("the riskfactor is not in the values, use the first value score as default.",
              call. = FALSE)
      idx <- 1
    }
    return(scores[idx])
  }

}

#' A wrapper of \code{\link{get_score}}
#'
#' Use lapply to a vector of risk factors and return a vector of scores.
#'
#' @param rf_vec A vector of risk factors
#' @inheritParams get_score
#'
#' @return A vector of score
#' @export
#' @rdname get_score
#' @examples
#' get_score_vec(c(57.5, 97.5,77), values, scores)
#'
get_score_vec <-function(rf_vec, values, scores) {
  unlist(
    lapply(rf_vec, get_score, values, scores)
    )
}

#' Raw risk factors value to score.
#'
#' The input risk factors should be a data.frame or a
#' \code{\link[tibble:as_tibble]{tibble}}, which contains all raw risk factors
#' value and should not contians user_id and others. The
#' \code{score_config_list} should be a list, \code{*_values} should be in front
#' of \code{*_scores}.
#'
#' You should carefull about the risk facotrs \code{rf_df} names and the
#' \code{score_config_list} names. \code{*_values}/\code{*_scores} and the risk
#' factors names should be the same.
#'
#' @param rf_df Risk factors data frame(exclude user_id)
#' @param score_config_list A List contains all riskFactors value and score
#'   index, read from .RDS file.
#' @author ZhaoWei
#' @export
#' @return a \code{\link[tibble:as_tibble]{tibble}} with every risk factor score
#' @examples
#' \dontrun{
#' require(dplyr)
#' score_config_list <- readRDS("result/score_config_list.RDS")
#' pj_rf <- readr::read_csv("data/pingjia/pingjia_result.csv",col_names = FALSE)
#' nameTmp <- c("acc_count_phk","act_radius","dec_count_phk","high_curv_tr","holiday_tr",
#' "interstate_r","lane_change_phk","late_night_tr","long_tr","main_act_prov"
#' , "mileage","speeding_lvl", "speeding_phk", "trip_dis_e", "turn_count_phk","user_id")
#' names(pj_rf) <- nameTmp
#'
#' risk_factor_names <- setdiff(nameTmp, c("user_id","main_act_prov"))
#' pj_rf <- pj_rf %>% select(!!risk_factor_names)
#'
#' rf2score(pj_rf, score_config_list)
#' }
#'
rf2score <- function(rf_df, score_config_list){
  if (nrow(rf_df) < 1) {
    stop("Input risk factors df is null")
  }
  rf_names <- names(rf_df)
  df_dim <- dim(rf_df)

  # if (length(score_config_list) / 2 != df_dim[2]) {
  #   stop(paste0("Please check your score_config_list which lenght should be ",
  #               2*df_dim[2], " but find ", length(score_config_list), " !"))
  # }
  check_names <-  unlist(lapply(rf_names, paste0, c("_values","_scores")))
  miss_names <- setdiff(check_names, names(score_config_list))
  if (length(miss_names) > 1) {
    stop("Please check score_config_list names which should contain all risk factors' names")
  }

  user_score <- matrix(data = NA, nrow = df_dim[1], ncol= df_dim[2])

  i <- 1
  for (s in rf_names) {
    tmp <- rf_df[[s]]
    idx <- grep(s, names(score_config_list))
    user_score[, i] <- get_score_vec(tmp, score_config_list[[idx[1]]],
                                          score_config_list[[idx[2]]])

    i <- i + 1
    print(paste("done:", s))
  }

  # factor to score
  colnames(user_score) <- rf_names

  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(user_score)
  }
  as.data.frame.matrix(user_score, stringsAsFactors = FALSE)
}


#' Sum every risk factor score
#'
#' You should carefull about the risk facotrs \code{rf_score} names and the
#' \code{rf_weight} names,they should be the same.
#'
#' @author ZhaoWei
#' @param rf_score The result of rf2score
#' @param rf_weight Risk factors weight, sum to 1.
#' @return A n*1 tibble/data.frame, the name is \code{score}.
#' @export
#'
#' @examples
#' \dontrun{
#' pj_rf <- readr::read_csv("data/pingjia/pingjia_result.csv",col_names = FALSE)
#' nameTmp <- c("acc_count_phk","act_radius","dec_count_phk","high_curv_tr","holiday_tr",
#'              "interstate_r","lane_change_phk","late_night_tr","long_tr","main_act_prov" ,
#'              "mileage","speeding_lvl", "speeding_phk", "trip_dis_e", "turn_count_phk", "user_id")
#' names(pj_rf) <- nameTmp
#'
#' user_id <- pj_rf[["user_id"]]
#' pj_rf <- select(pj_rf, -user_id)
#'
#' score_config_list <- readRDS("result/score_config_list.RDS")
#' rf_score <- rf2score(pj_rf, score_config_list)
#'
#' rf_weight <- readRDS("result/rf_weight.RDS")
#' sum_rf_score(rf_score, rf_weight)
#'
#' }
#'
sum_rf_score <- function(rf_score, rf_weight){
  n_col <- ncol(rf_score)
  m_row <- nrow(rf_weight)
  stopifnot(n_col == m_row)

  if (!is.character(rf_weight[[1]])) {
    stop("rf_weight first column should be risk factor name")
  }

  weight_names <- rf_weight[[1]]
  idx <- integer(n_col)
  i <- 1L
  for (name in names(rf_score)) {
    idx[i] <- grep(name, weight_names, perl = TRUE)
    i <- i + 1
  }
  wt <- rf_weight[[2]][idx]
  sum_score <- as.matrix(rf_score) %*% wt
  colnames(sum_score) <- "score"

  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(sum_score)
  }
  as.data.frame.matrix(sum_score, stringsAsFactors = FALSE)

}
