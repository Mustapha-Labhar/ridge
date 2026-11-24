
test_that("ridge_cv_cpp rejects a single fold", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  expect_error(
    ridge:::ridge_cv_cpp(
      x,
      y,
      c(0.1, 1),
      rep(1L, 6)
    ),
    "At least two folds"
  )
})


test_that("ridge_cv_cpp rejects fold identifiers greater than n", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  expect_error(
    ridge:::ridge_cv_cpp(
      x,
      y,
      c(0.1, 1),
      c(1L, 2L, 3L, 4L, 5L, 7L)
    ),
    "cannot exceed"
  )
})


test_that("ridge_cv_cpp rejects non-consecutive fold identifiers", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  expect_error(
    ridge:::ridge_cv_cpp(
      x,
      y,
      c(0.1, 1),
      c(1L, 1L, 3L, 3L, 3L, 3L)
    ),
    "consecutive integers"
  )
})




test_that("ridge_cv_cpp validates input dimensions and lengths", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- as.numeric(seq_len(6))
  lambdas <- c(0.1, 1)
  folds <- rep(1:3, each = 2)

  # At least two observations and one predictor
  x_too_small <- matrix(1, nrow = 1, ncol = 2)

  expect_error(
    ridge:::ridge_cv_cpp(x_too_small, y[1], lambdas, 1L),
    "at least two observations"
  )

  # Response length
  expect_error(
    ridge:::ridge_cv_cpp(x, y[-1], lambdas, folds),
    "same number of observations"
  )

  # Fold assignment length
  expect_error(
    ridge:::ridge_cv_cpp(x, y, lambdas, folds[-1]),
    "same length"
  )

  # Empty lambda grid
  expect_error(
    ridge:::ridge_cv_cpp(x, y, numeric(0), folds),
    "at least one value"
  )
})


test_that("ridge_cv_cpp rejects non-finite input values", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- as.numeric(seq_len(6))
  lambdas <- c(0.1, 1)
  folds <- rep(1:3, each = 2)

  # Non-finite response
  y_bad <- y
  y_bad[1] <- NA_real_

  expect_error(
    ridge:::ridge_cv_cpp(x, y_bad, lambdas, folds),
    "finite values"
  )

  # Non-finite predictor
  x_bad <- x
  x_bad[1, 1] <- Inf

  expect_error(
    ridge:::ridge_cv_cpp(x_bad, y, lambdas, folds),
    "finite values"
  )

  # Invalid lambda
  expect_error(
    ridge:::ridge_cv_cpp(x, y, c(0, 1), folds),
    "strictly positive and finite"
  )
})


test_that("ridge_cv_cpp rejects non-positive fold identifiers", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- as.numeric(seq_len(6))
  lambdas <- c(0.1, 1)

  folds_bad <- c(0L, 1L, 1L, 2L, 2L, 3L)

  expect_error(
    ridge:::ridge_cv_cpp(x, y, lambdas, folds_bad),
    "positive integers"
  )
})


