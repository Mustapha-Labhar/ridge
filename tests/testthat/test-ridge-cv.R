
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



test_that("ridge_cv assigns every observation to exactly one fold", {

  set.seed(456)

  x <- matrix(rnorm(202), nrow = 101, ncol = 2)
  y <- rnorm(101)

  result <- ridge_cv(
    x,
    y,
    lambda_grid = c(0.01, 0.1, 1, 10),
    k = 5,
    seed = 123
  )

  fold_sizes <- tabulate(result$fold_id, nbins = 5)

  expect_length(result$fold_id, 101)
  expect_true(all(result$fold_id %in% 1:5))
  expect_equal(sum(fold_sizes), 101)
  expect_true(max(fold_sizes) - min(fold_sizes) <= 1)
})


test_that("ridge_cv computes consistent cross-validation errors", {

  set.seed(456)

  x <- matrix(rnorm(120), nrow = 40, ncol = 3)
  y <- rnorm(40)

  result <- ridge_cv(
    x,
    y,
    lambda_grid = c(0.01, 0.1, 1, 10),
    k = 5,
    seed = 123
  )

  fold_sizes <- tabulate(result$fold_id, nbins = 5)

  expected_pooled <- colSums(
    sweep(result$fold_errors, 1, fold_sizes, "*")
  ) / nrow(x)

  expected_mean_fold <- colMeans(result$fold_errors)

  expect_equal(
    unname(result$pooled_cv_errors),
    unname(expected_pooled),
    tolerance = 1e-12
  )

  expect_equal(
    unname(result$mean_fold_cv_errors),
    unname(expected_mean_fold),
    tolerance = 1e-12
  )
})


test_that("ridge_cv selects the lambda minimizing pooled CV error", {

  set.seed(789)

  x <- matrix(rnorm(200), nrow = 100, ncol = 2)
  y <- 2 + x[, 1] - 2 * x[, 2] + rnorm(100)

  result <- ridge_cv(
    x,
    y,
    lambda_grid = 10^seq(-3, 2, length.out = 20),
    k = 5,
    seed = 123
  )

  expect_equal(
    result$best_mse,
    min(result$pooled_cv_errors),
    tolerance = 1e-12
  )

  expect_identical(
    result$best_index,
    as.integer(which.min(result$pooled_cv_errors))
  )

  expect_equal(
    result$best_lambda,
    result$lambda_grid[result$best_index]
  )
})


test_that("ridge_cv preserves the user's random-number state", {

  set.seed(2026)

  seed_before <- .Random.seed

  x <- matrix(rnorm(80), nrow = 40, ncol = 2)
  y <- rnorm(40)

  seed_before <- .Random.seed

  ridge_cv(
    x,
    y,
    lambda_grid = c(0.01, 0.1, 1),
    k = 4,
    seed = 123
  )

  expect_identical(.Random.seed, seed_before)
})






test_that("ridge_cv rejects non-finite or invalid x", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  x_invalid <- x
  x_invalid[1, 1] <- Inf

  expect_error(
    ridge_cv(x_invalid, y, c(0.1, 1)),
    "finite numeric matrix"
  )
})


test_that("ridge_cv rejects invalid y", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)

  expect_error(
    ridge_cv(x, rep(NA_real_, 6), c(0.1, 1)),
    "finite numeric vector"
  )
})


test_that("ridge_cv rejects a response with the wrong length", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(5)

  expect_error(
    ridge_cv(x, y, c(0.1, 1)),
    "same number of observations"
  )
})


test_that("ridge_cv rejects an invalid seed", {

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  expect_error(
    ridge_cv(x, y, c(0.1, 1), seed = 1.5),
    "seed must be a valid integer"
  )
})


test_that("ridge_cv preserves the absence of the initial random seed", {

  had_seed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
  }

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv,
                      inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  if (exists(".Random.seed", envir = .GlobalEnv,
             inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  x <- matrix(seq_len(12), nrow = 6, ncol = 2)
  y <- seq_len(6)

  result <- ridge_cv(x, y, c(0.1, 1), k = 3, seed = 123)

  expect_s3_class(result, "ridge_cv")
  expect_false(
    exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  )
})



