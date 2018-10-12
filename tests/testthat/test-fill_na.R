context("test-fill_na.R")

test_that("fill_na works", {
  x <- c(1, NA, NA, 2, 3)
  y <- c(NA, NA, 1, NA, 3)
  x_r <- c(1, 1, 1, 2, 3)
  y_r <- c(1, 1, 1, 1, 3)
  expect_equal(x_r, fill_na(x))
  expect_equal(y_r, fill_na(y))
  xy <- data.frame(x,y)
  xy_r <- data.frame(x = x_r, y = y_r)
  expect_equal(xy_r,fill_na(xy, x, y))
  expect_equal(xy_r, fill_na(xy, c("x","y")))
  #tibble
  xy <- tibble::as_tibble(xy)
  xy_r <- tibble::as_tibble(xy_r)
  expect_equal(xy_r,fill_na(xy, x, y))
  expect_equal(xy_r, fill_na(xy, c("x","y")))
})
