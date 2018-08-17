## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----load-pkg------------------------------------------------------------
library(pjutils)
library(tibble)

## ----demo-bs-------------------------------------------------------------
# binary search
binary_search_r(1.3, 1:10)
binary_search_cpp(1.3, 1:10)

binary_search_r(9.9, 1:10)
binary_search_cpp(9.9, 1:10)

binary_search_r(0.9, 1:10)
binary_search_cpp(0.9, 1:10)

binary_search_r(10.9, 1:10)
binary_search_cpp(10.9, 1:10)

## ----demo-getScore-------------------------------------------------------
# get score
values <- c(55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
scores <- c(1.0000, 0.9747, 0.8967, 0.8303, 0.7730, 0.7232, 0.6793, 0.6405, 
            0.6059, 0.5748)
get_score(55, values, scores)
get_score(57.5, values, scores)
get_score(97.5, values, scores)
get_score(77, values, scores)
get_score(100, values, scores)
get_score(101, values, scores)

get_score_vec(c(55, 57.5, 97.5, 77, 100, 101), values, scores)


## ----benchmark, cache=TRUE, fig.width=7, fig.height=5, fig.align='center'----
set.seed(12345)
aa <- runif(100000, 1, 1000)
bb <- microbenchmark::microbenchmark(
  r = lapply(aa, binary_search_r, y = 1:1000),
  cpp = lapply(aa, binary_search_cpp, y = 1000),
  times = 10L
)
bb

microbenchmark::autoplot.microbenchmark(bb)


## ----demo-magrittr-------------------------------------------------------
library(dplyr)
tmp <- tibble(
  acc_count_phk   = 5.281214,
  act_radius      = 513765.4,
  day_mileage     = 12345,
  dec_count_phk   = 3.492416,
  high_curv_tr    = 0.06097561,
  holiday_tr      = 0.07058824,
  interstate_r    = 0      ,
  lane_change_phk = 2.108227,
  late_night_tr   = 0.01764706,
  long_tr         = 0.002941176,
  main_act_prov   = "½­ËÕÊ¡",
  mileage         = 14077.86,
  speeding_lvl    = 0      ,
  speeding_phk    = 24.97929,
  trip_dis_e      = 3.359256,
  turn_count_phk  = 3.300759,
  user_id         = 2069
)
tmp

# read config file
score_config_list <- readRDS("E:/A-r-wd/1-test/result/score_config_list.RDS")
rf_weight <- readRDS("E:/A-r-wd/1-test/result/rf_weight.RDS")
discount_index <- readRDS("E:/A-r-wd/1-test/result/dicount_index.RDS")

risk_names <- c("mileage","speeding_phk","speeding_lvl","acc_count_phk",
                "dec_count_phk","turn_count_phk","lane_change_phk","long_tr",
                "late_night_tr","holiday_tr","high_curv_tr","trip_dis_e",
                "act_radius", "interstate_r","main_act_prov")

tmp %>% 
  # drop out other cols
  select(-user_id, -day_mileage) %>% 
  # get every risk factor specified score
  rf2score(score_config_list = score_config_list) %>% 
  # sum all risk factors socre by weight
  sum_rf_score(rf_weight) %>%
  .$score %>%
  # get score specified discount coef
  get_score_vec(discount_index$score, discount_index$coef)

# all in one
result <- cal_user_coef(tmp, risk_names, score_config_list, rf_weight, discount_index)

## ----result-table, echo=FALSE--------------------------------------------
knitr::kable(result)

