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
