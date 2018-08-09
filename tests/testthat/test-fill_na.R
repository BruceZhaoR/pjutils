context("test-fill_na.R")

test_that("multiplication works", {
  x <- c(1, NA, NA, 2, 3)
  y <- c(NA, NA, 1, NA, 3)
  expect_equal(c(1,1,1,2,3), fill_na(x))
  expect_equal(c(1,1,1,1,3), fill_na(y))

})
