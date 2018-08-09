context("test-binary-search.R")

test_that("r binary search works", {
  expect_identical(2L, binary_search_r(1.3, 1:10))
  expect_identical(1L, binary_search_cpp(1.3, 1:10))
})
