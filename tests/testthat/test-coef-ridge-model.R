test_that("coef.ridge_model returns the model coefficients", {

  x <- matrix(
    c(1, 2, 3, 4, 5, 6),
    ncol = 1
  )

  y <- c(3, 5, 7, 9, 11, 13)

  model <- ridge_fit(x, y, lambda = 1)

  result <- coef(model)

  expect_type(result, "double")
  expect_length(result, 2)
  expect_named(result, c("(Intercept)", "x1"))
  expect_equal(result, model$coefficients)

})