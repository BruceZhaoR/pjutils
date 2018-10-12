context("test-utils")

test_that("auc works", {
  actual <- c(1, 1, 1, 0, 0, 0)
  predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
  expect_true( abs(auc(actual, predicted) - 0.88888) < 0.00001)
})
