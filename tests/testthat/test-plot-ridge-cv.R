test_that("plot.ridge_cv returns the selected parameters", {

  set.seed(123)

  x <- matrix(rnorm(200), ncol = 2)

  y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)

  lambda_grid <- 10^seq(-3, 2, length.out = 10)

  result <- ridge_cv(
    x = x,
    y = y,
    lambda_grid = lambda_grid,
    k = 5,
    seed = 123
  )

  selection <- plot.ridge_cv(result)

  expect_type(selection, "double")

  expect_named(
    selection,
    c(
      "lambda_min",
      "lambda_1se",
      "min_error",
      "se_min",
      "threshold_1se"
    )
  )

  expect_length(selection, 5)

})

test_that("plot.ridge_cv computes the selected parameters correctly", {

  set.seed(123)

  x <- matrix(rnorm(200), ncol = 2)

  y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)

  lambda_grid <- 10^seq(-3, 2, length.out = 10)

  result <- ridge_cv(
    x = x,
    y = y,
    lambda_grid = lambda_grid,
    k = 5,
    seed = 123
  )

  selection <- plot.ridge_cv(result)

  expect_equal(
    unname(selection["lambda_min"]),
    0.0464158883,
    tolerance = 1e-7
  )

  expect_equal(
    unname(selection["lambda_1se"]),
    7.742636827,
    tolerance = 1e-7
  )

  expect_equal(
    unname(selection["min_error"]),
    0.92702703,
    tolerance = 1e-6
  )

})


test_that("plot.ridge_cv rejects invalid objects", {

  expect_error(
    plot.ridge_cv(list(a = 1, b = 2)),
    "x must be an object of class 'ridge_cv'."
  )

  expect_error(
    plot.ridge_cv(123),
    "x must be an object of class 'ridge_cv'."
  )

  expect_error(
    plot.ridge_cv(NULL),
    "x must be an object of class 'ridge_cv'."
  )

})


test_that("plot.ridge_cv produces a graphical output", {

  set.seed(123)

  x <- matrix(rnorm(200), ncol = 2)

  y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)

  lambda_grid <- 10^seq(-3, 2, length.out = 10)

  result <- ridge_cv(
    x = x,
    y = y,
    lambda_grid = lambda_grid,
    k = 5,
    seed = 123
  )

  pdf(NULL)

  expect_no_error(
    plot.ridge_cv(result)
  )

  dev.off()

})


test_that("plot.ridge_cv accepts additional graphical parameters", {

  set.seed(123)

  x <- matrix(rnorm(200), ncol = 2)

  y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)

  lambda_grid <- 10^seq(-3, 2, length.out = 10)

  result <- ridge_cv(
    x = x,
    y = y,
    lambda_grid = lambda_grid,
    k = 5,
    seed = 123
  )

  grDevices::pdf(NULL)

  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    plot.ridge_cv(
      result,
      col = "blue",
      pch = 17,
      cex = 1.2
    )
  )

})
