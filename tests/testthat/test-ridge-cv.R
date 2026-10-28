
test_that("ridge_cv returns a valid result", {
  set.seed(123)

  x <- matrix(rnorm(80), nrow = 40, ncol = 2)
  y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(40)

  lambda_grid <- 10^seq(-3, 2, length.out = 10)

  result <- ridge_cv(
    x = x,
    y = y,
    lambda_grid = lambda_grid,
    k = 4,
    seed = 123
  )

  expect_s3_class(result, "ridge_cv")
  expect_length(result$best_lambda, 1)
  expect_true(result$best_lambda %in% lambda_grid)
  expect_length(result$mean_cv_errors, length(lambda_grid))
  expect_true(all(is.finite(result$mean_cv_errors)))
  expect_true(all(result$mean_cv_errors >= 0))
})

test_that("ridge_cv rejects invalid lambda values", {
  x <- matrix(rnorm(40), nrow = 20, ncol = 2)
  y <- rnorm(20)

  expect_error(
    ridge_cv(x, y, lambda_grid = c(0, 1)),
    "strictly positive"
  )
})

test_that("ridge_cv rejects an invalid number of folds", {
  x <- matrix(rnorm(40), nrow = 20, ncol = 2)
  y <- rnorm(20)

  expect_error(
    ridge_cv(x, y, lambda_grid = c(0.1, 1), k = 1),
    "integer between 2 and nrow"
  )
})
