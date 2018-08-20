---
title: "A Demo of Risk Factors To Score"
pkgdown:
  as_is: true
author: "Wei Zhao"
date: "2018-08-20"
output: 
  prettydoc::html_pretty:
    theme: tactile
    highlight: github
    toc: true
    number_sections: true
    mathjax: NULL
    self_contained: true
    keep_md: true
    fig_retina: 2
vignette: >
  %\VignetteIndexEntry{risk factors to score demo}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



**For the best result printing, let't `library(tibble)` or `dplyr`.**    



```r
library(pjutils)
#> Welcome to the package, developing by Wei Zhao, Pingjia Technology. 
#> If you have any question, please email `zhaowei@chinaubi.com`.
library(tibble)
```

# A demo of using the functions     

The binary search have two version:    

- r version, start from **`1`**, which if faster when the `length(y)` is large than 1e5.
- cpp version start form **`0`**, which if faster when `length(y)` is small than 1e5.


```r
# binary search
binary_search_r(1.3, 1:10)
#> [1] 2
binary_search_cpp(1.3, 1:10)
#> [1] 1

binary_search_r(9.9, 1:10)
#> [1] 10
binary_search_cpp(9.9, 1:10)
#> [1] 9

binary_search_r(0.9, 1:10)
#> [1] 1
binary_search_cpp(0.9, 1:10)
#> [1] 0

binary_search_r(10.9, 1:10)
#> [1] 11
binary_search_cpp(10.9, 1:10)
#> [1] 10
```

The `get_score` apply to a sigle value while `get_socre_vec` can apply to a
vector of vaues: 


```r
# get score
values <- c(55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
scores <- c(1.0000, 0.9747, 0.8967, 0.8303, 0.7730, 0.7232, 0.6793, 0.6405, 
            0.6059, 0.5748)
get_score(55, values, scores)
#> [1] 1
get_score(57.5, values, scores)
#> [1] 0.98735
get_score(97.5, values, scores)
#> [1] 0.59035
get_score(77, values, scores)
#> [1] 0.75308
get_score(100, values, scores)
#> [1] 0.5748
get_score(101, values, scores)
#> [1] 0

get_score_vec(c(55, 57.5, 97.5, 77, 100, 101), values, scores)
#> [1] 1.00000 0.98735 0.59035 0.75308 0.57480 0.00000
```

# Benchmark


```r
set.seed(12345)
aa <- runif(100000, 1, 1000)
bb <- microbenchmark::microbenchmark(
  r = lapply(aa, binary_search_r, y = 1:1000),
  cpp = lapply(aa, binary_search_cpp, y = 1000),
  times = 10L
)
bb
#> Unit: milliseconds
#>  expr       min       lq      mean    median        uq      max neval cld
#>     r 2451.7975 2489.443 2555.7164 2557.0141 2621.2185 2645.566    10   b
#>   cpp  193.6113  197.090  210.8907  215.0455  220.1943  225.659    10  a

microbenchmark::autoplot.microbenchmark(bb)
#> Loading required namespace: ggplot2
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

<img src="E:/A-pkg-dev/pjutils/docs/articles/risk-factors-to-score_files/figure-html/benchmark-1.png" width="100%" style="display: block; margin: auto;" />

# A tidyverse workflow


```r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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
  main_act_prov   = "江苏省",
  mileage         = 14077.86,
  speeding_lvl    = 0      ,
  speeding_phk    = 24.97929,
  trip_dis_e      = 3.359256,
  turn_count_phk  = 3.300759,
  user_id         = 2069
)
tmp
#> # A tibble: 1 x 17
#>   acc_count_phk act_radius day_mileage dec_count_phk high_curv_tr
#>           <dbl>      <dbl>       <dbl>         <dbl>        <dbl>
#> 1          5.28    513765.       12345          3.49       0.0610
#> # ... with 12 more variables: holiday_tr <dbl>, interstate_r <dbl>,
#> #   lane_change_phk <dbl>, late_night_tr <dbl>, long_tr <dbl>,
#> #   main_act_prov <chr>, mileage <dbl>, speeding_lvl <dbl>,
#> #   speeding_phk <dbl>, trip_dis_e <dbl>, turn_count_phk <dbl>,
#> #   user_id <dbl>

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
#> [1] "done: acc_count_phk"
#> [1] "done: act_radius"
#> [1] "done: dec_count_phk"
#> [1] "done: high_curv_tr"
#> [1] "done: holiday_tr"
#> [1] "done: interstate_r"
#> [1] "done: lane_change_phk"
#> [1] "done: late_night_tr"
#> [1] "done: long_tr"
#> [1] "done: main_act_prov"
#> [1] "done: mileage"
#> [1] "done: speeding_lvl"
#> [1] "done: speeding_phk"
#> [1] "done: trip_dis_e"
#> [1] "done: turn_count_phk"
#> [1] 0.8563817

# all in one
result <- cal_user_coef(tmp, risk_names, score_config_list, rf_weight, discount_index)
#> [1] "done: mileage"
#> [1] "done: speeding_phk"
#> [1] "done: speeding_lvl"
#> [1] "done: acc_count_phk"
#> [1] "done: dec_count_phk"
#> [1] "done: turn_count_phk"
#> [1] "done: lane_change_phk"
#> [1] "done: long_tr"
#> [1] "done: late_night_tr"
#> [1] "done: holiday_tr"
#> [1] "done: high_curv_tr"
#> [1] "done: trip_dis_e"
#> [1] "done: act_radius"
#> [1] "done: interstate_r"
#> [1] "done: main_act_prov"
```

Here is the result table:


 score   day_mileage   discount_coef   discount   delta
------  ------------  --------------  ---------  ------
 68.04         12345          0.8563     0.1437       0

<p style="text-align: center;margin-bottom:-5px;"> That's the end of doc! &nbsp;&nbsp; (我是有底线的 ^_^)</p>
-------------------------------------------------------------------------------
