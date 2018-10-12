context("test-get-score.R")

test_that("get_score works", {
  values <- c(55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
  scores <- c(1.0000, 0.9747, 0.8967, 0.8303, 0.7730, 0.7232, 0.6793, 0.6405,
              0.6059, 0.5748)
  expect_equal(1.00, get_score(55, values, scores))
  expect_equal(0.98735, get_score(57.5, values, scores))
  expect_equal(0.59035, get_score(97.5, values, scores))
  expect_equal(0.75308, get_score(77, values, scores))
  expect_equal(0.5748, get_score(100, values, scores))
  expect_equal(0, get_score(101, values, scores))
  expect_equal(1.00, get_score(54, values, scores))

  expect_equal(
    c(1, 0.98735, 0.59035, 0.75308, 0.5748, 0),
    get_score_vec(c(55, 57.5, 97.5, 77, 100, 101), values, scores)
  )

  values <- c("abc","fac","gbk","lgb","xgb","gbm","rf","lasso","ridge","eln")
  expect_equal(1, get_score("abc", values, scores))
  expect_equal(0.5748, get_score("eln", values, scores))
  expect_equal(0.8303, get_score("lgb", values, scores))
  expect_warning(get_score("fck", values, scores))
})
