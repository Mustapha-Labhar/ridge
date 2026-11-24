
test_that("residuals.ridge_model returns the residuals", {

  set.seed(123)

  x <- matrix(rnorm(100), ncol = 2)
  y <- 2 + 3 * x[, 1] - 1.5 * x[, 2] + rnorm(50)

  model <- ridge_fit(x, y, lambda = 1)

  result <- residuals(model)

  expect_type(result, "double")
  expect_length(result, length(y))
  expect_true(all(is.finite(result)))
  expect_equal(result, model$residuals)

  # Verify the mathematical identity:
  # y = fitted values + residuals

  expect_equal(
    as.numeric(y),
    fitted(model) + result
  )
})


test_that("residuals.ridge_model rejects objects of incorrect class", {

  expect_error(
    ridge:::residuals.ridge_model(
      list(coefficients = c(1, 2))
    ),
    "must inherit from class ridge_model"
  )

})
