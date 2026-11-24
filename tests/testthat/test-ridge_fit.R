test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("ridge_fit rejects invalid lambda", {
  x <- matrix(rnorm(20), ncol = 2)
  y <- rnorm(10)

  expect_error(
    ridge_fit(x, y, lambda = -1),
    "lambda must be a single non-negative numeric value."
  )

  expect_error(
    ridge_fit(x, y, lambda = c(1, 2)),
    "lambda must be a single non-negative numeric value."
  )
})


test_that("ridge_fit rejects incompatible dimensions", {
  x <- matrix(rnorm(20), ncol = 2)
  y <- rnorm(9)

  expect_error(
    ridge_fit(x, y),
    "y must have the same number of observations as x."
  )
})


test_that("ridge_fit rejects non-numeric data", {
  x <- matrix(rnorm(20), ncol = 2)
  y <- rnorm(10)

  expect_error(
    ridge_fit(x, as.character(y)),
    "y must be numeric."
  )

  expect_error(
    ridge_fit(matrix(as.character(x), ncol = 2), y),
    "x must be numeric."
  )
})


test_that("ridge_fit agrees with OLS when lambda is zero", {
  x <- matrix(c(
    1, 2,
    2, 3,
    3, 5,
    4, 7,
    5, 11
  ), ncol = 2, byrow = TRUE)

  y <- c(3, 5, 8, 11, 17)

  ridge_model <- ridge_fit(x, y, lambda = 0)

  ols_model <- lm(y ~ x)

  expect_equal(
    unname(ridge_model$coefficients),
    unname(coef(ols_model)),
    tolerance = 1e-10
  )

})


test_that("ridge does not penalize the intercept", {
  x <- matrix(1:5, ncol = 1)
  y <- c(12, 14, 16, 18, 20)

  model <- ridge_fit(x, y, lambda = 1000)

  expect_true(abs(model$coefficients[1]) > 10)
  expect_true(abs(model$coefficients[2]) < 1)
})


test_that("ridge_fit returns named coefficients", {
  x <- matrix(
    c(1, 2,
      2, 3,
      3, 5,
      4, 7,
      5, 11),
    ncol = 2,
    byrow = TRUE
  )

  y <- c(3, 5, 8, 11, 17)

  model <- ridge_fit(x, y, lambda = 1)

  expect_named(
    model$coefficients,
    c("(Intercept)", "x1", "x2")
  )
})


test_that("ridge_fit preserves predictor names", {
  x <- matrix(
    c(1, 2,
      2, 3,
      3, 5,
      4, 7,
      5, 11),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("age", "income"))
  )

  y <- c(3, 5, 8, 11, 17)

  model <- ridge_fit(x, y, lambda = 1)

  expect_named(
    model$coefficients,
    c("(Intercept)", "age", "income")
  )
})





test_that("ridge rejects non-finite predictors", {

  x <- matrix(c(1, 2, 3, 4), ncol = 1)
  y <- c(2, 4, 6, 8)

  x_na <- x
  x_na[1, 1] <- NA_real_

  x_inf <- x
  x_inf[1, 1] <- Inf

  expect_error(ridge(x_na, y))
  expect_error(ridge(x_inf, y))
})


test_that("ridge rejects non-finite responses", {

  x <- matrix(1:4, ncol = 1)

  y_na <- c(2, NA_real_, 6, 8)
  y_inf <- c(2, 4, Inf, 8)

  expect_error(ridge(x, y_na))
  expect_error(ridge(x, y_inf))
})


test_that("ridge rejects invalid lambda values", {

  x <- matrix(1:4, ncol = 1)
  y <- c(2, 4, 6, 8)

  expect_error(ridge(x, y, lambda = Inf))
  expect_error(ridge(x, y, lambda = NaN))
})


test_that("ridge rejects empty predictor matrices", {

  x <- matrix(numeric(0), nrow = 4, ncol = 0)
  y <- c(2, 4, 6, 8)

  expect_error(ridge(x, y))
})


test_that("ridge handles perfectly collinear predictors with positive lambda", {

  x <- cbind(
    x1 = 1:10,
    x2 = 2 * (1:10)
  )

  y <- 3 + 2 * x[, 1]

  model <- ridge(x, y, lambda = 1)

  expect_true(all(is.finite(model$coefficients)))
  expect_named(
    model$coefficients,
    c("(Intercept)", "x1", "x2")
  )
})




test_that("ridge_fit accepts a numeric vector as x", {

  x <- 1:10
  y <- 2 + 3 * x

  model <- ridge_fit(x, y, lambda = 1)

  expect_s3_class(model, "ridge_model")
  expect_length(model$coefficients, 2)
})


test_that("ridge_fit rejects a non-vector response", {

  x <- matrix(seq_len(12), ncol = 2)
  y <- matrix(seq_len(6), nrow = 3)

  expect_error(
    ridge_fit(x, y, lambda = 1),
    "y must be a numeric vector"
  )
})
