
test_that("fitted.ridge_model returns the fitted values", {

  set.seed(123)

  x <- matrix(rnorm(100), ncol = 2)
  y <- 2 + 3 * x[, 1] - 1.5 * x[, 2] + rnorm(50)

  model <- ridge_fit(x, y, lambda = 1)

  result <- fitted(model)

  expect_type(result, "double")
  expect_length(result, length(y))
  expect_true(all(is.finite(result)))
  expect_equal(result, model$fitted.values)
})


test_that("fitted.ridge_model rejects objects of incorrect class", {

  expect_error(
    ridge:::fitted.ridge_model(
      list(coefficients = c(1, 2))
    ),
    "must inherit from class ridge_model"
  )

})
