
test_that("new_ridge_model creates a valid ridge_model object", {

  model <- new_ridge_model(
    coefficients = c(
      "(Intercept)" = 2,
      "x1" = 1.5
    ),
    lambda = 1,
    fitted_values = c(3, 4),
    residuals = c(0.2, -0.1)
  )

  expect_s3_class(model, "ridge_model")
  expect_named(
    model$coefficients,
    c("(Intercept)", "x1")
  )
  expect_equal(model$lambda, 1)
  expect_length(model$fitted.values, 2)
  expect_length(model$residuals, 2)
})



test_that("new_ridge_model rejects invalid coefficients", {

  expect_error(
    new_ridge_model(
      coefficients = c(2, 1.5),
      lambda = 1,
      fitted_values = c(3, 4),
      residuals = c(0.2, -0.1)
    ),
    "unique, non-empty names"
  )
})


test_that("new_ridge_model rejects negative lambda", {

  expect_error(
    new_ridge_model(
      coefficients = c("(Intercept)" = 2, x1 = 1.5),
      lambda = -1,
      fitted_values = c(3, 4),
      residuals = c(0.2, -0.1)
    ),
    "non-negative"
  )
})


test_that("new_ridge_model rejects empty fitted values", {

  expect_error(
    new_ridge_model(
      coefficients = c("(Intercept)" = 2, x1 = 1.5),
      lambda = 1,
      fitted_values = numeric(0),
      residuals = numeric(0)
    ),
    "non-empty"
  )
})


test_that("new_ridge_model rejects inconsistent vector lengths", {

  expect_error(
    new_ridge_model(
      coefficients = c("(Intercept)" = 2, x1 = 1.5),
      lambda = 1,
      fitted_values = c(3, 4),
      residuals = c(0.2)
    ),
    "same length"
  )
})


