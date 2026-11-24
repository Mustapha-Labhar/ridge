
test_that("predict.ridge_model returns numeric predictions", {

  x <- matrix(rnorm(40), ncol = 2)
  y <- rnorm(20)

  model <- ridge_fit(x, y, lambda = 1)

  newdata <- matrix(rnorm(10), ncol = 2)

  result <- predict(model, newdata = newdata)

  expect_type(result, "double")
  expect_length(result, nrow(newdata))
  expect_true(all(is.finite(result)))

})


test_that("predict.ridge_model handles named predictors", {

  x <- matrix(
    rnorm(40),
    ncol = 2,
    dimnames = list(NULL, c("temperature", "pression"))
  )

  y <- rnorm(20)

  model <- ridge_fit(x, y, lambda = 1)

  newdata <- matrix(
    rnorm(10),
    ncol = 2,
    dimnames = list(NULL, c("pression", "temperature"))
  )

  result <- predict(model, newdata = newdata)

  expect_length(result, nrow(newdata))
  expect_true(all(is.finite(result)))

})


test_that("predict.ridge_model rejects incorrect dimensions", {

  x <- matrix(rnorm(40), ncol = 2)
  y <- rnorm(20)

  model <- ridge_fit(x, y, lambda = 1)

  newdata <- matrix(rnorm(10), ncol = 1)

  expect_error(
    predict(model, newdata = newdata),
    "same number of predictors"
  )

})


test_that("predict.ridge_model rejects missing or non-finite data", {

  x <- matrix(rnorm(40), ncol = 2)
  y <- rnorm(20)

  model <- ridge_fit(x, y, lambda = 1)

  newdata <- matrix(rnorm(10), ncol = 2)
  newdata[1, 1] <- NA_real_

  expect_error(
    predict(model, newdata = newdata),
    "finite values"
  )

})


test_that("predict.ridge_model rejects mismatched names", {

  x <- matrix(
    rnorm(40),
    ncol = 2,
    dimnames = list(NULL, c("temperature", "pression"))
  )

  y <- rnorm(20)

  model <- ridge_fit(x, y, lambda = 1)

  newdata <- matrix(
    rnorm(10),
    ncol = 2,
    dimnames = list(NULL, c("temperature", "humidite"))
  )

  expect_error(
    predict(model, newdata = newdata),
    "column names must match"
  )

})
