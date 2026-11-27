
#' Select the ridge regularization parameter using cross-validation
#'
#' Selects the regularization parameter using k-fold cross-validation
#' performed by the C++ implementation.
#'
#' @param x A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @param lambda_grid A numeric vector of strictly positive
#'   regularization parameters to evaluate.
#' @param k Number of folds for cross-validation.
#' @param seed Random seed for reproducibility.
#'
#' @return An object of class "ridge_cv" containing the selected
#'   lambda, cross-validation errors, fold-specific errors,
#'   coefficients, and fold assignments.
#'
#' @examples
#' x <- matrix(rnorm(80), ncol = 2)
#' y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(40)
#' result <- ridge_cv(x, y, 10^seq(-3, 2, length.out = 10))
#' result$best_lambda
#'
#' @export
ridge_cv <- function(x, y, lambda_grid, k = 5, seed = 123) {

  # ----------------------------------------------------------
  # 1. Argument validation
  # ----------------------------------------------------------

  x <- as.matrix(x)

  if (!is.numeric(x) ||
      length(dim(x)) != 2L ||
      nrow(x) < 2L ||
      ncol(x) < 1L ||
      any(!is.finite(x))) {
    stop("x must be a finite numeric matrix with predictors.")
  }

  if (!is.numeric(y) ||
      !is.null(dim(y)) ||
      any(!is.finite(y))) {
    stop("y must be a finite numeric vector.")
  }

  if (length(y) != nrow(x)) {
    stop("y must have the same number of observations as x.")
  }

  if (length(k) != 1L ||
      !is.numeric(k) ||
      !is.finite(k) ||
      k != floor(k) ||
      k < 2L ||
      k > nrow(x)) {
    stop("k must be an integer between 2 and nrow(x).")
  }

  if (
    !is.numeric(lambda_grid) ||
    !is.null(dim(lambda_grid)) ||
    length(lambda_grid) == 0L ||
    any(!is.finite(lambda_grid)) ||
    any(lambda_grid <= 0) ||
    anyDuplicated(lambda_grid) ||
    is.unsorted(lambda_grid)
  ) {
    stop(
      "lambda_grid must be a strictly increasing numeric vector ",
      "of strictly positive finite values without duplicates."
    )
  }

  if (length(seed) != 1L ||
      !is.numeric(seed) ||
      !is.finite(seed) ||
      seed != floor(seed) ||
      abs(seed) > .Machine$integer.max) {
    stop("seed must be a valid integer.")
  }

  # ----------------------------------------------------------
  # 2. Prepare data for C++
  # ----------------------------------------------------------

  storage.mode(x) <- "double"
  y <- as.numeric(y)

  # ----------------------------------------------------------
  # 3. Generate reproducible folds
  #    without modifying the user's random-number state
  # ----------------------------------------------------------

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

    } else if (
      exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    ) {
      rm(".Random.seed", envir = .GlobalEnv)
    }

  }, add = TRUE)

  set.seed(as.integer(seed))

  n <- nrow(x)

  fold_id <- sample(
    rep(seq_len(k), length.out = n)
  )

  # ----------------------------------------------------------
  # 4. Execute cross-validation entirely in C++
  # ----------------------------------------------------------

  cpp_result <- ridge_cv_cpp(
    x = x,
    y = y,
    lambdas = lambda_grid,
    fold_id = fold_id
  )

  # ----------------------------------------------------------
  # 5. Organize results
  # ----------------------------------------------------------

  lambda_names <- paste0(
    "lambda_",
    format(lambda_grid, scientific = TRUE)
  )

  mean_cv_errors <- cpp_result$pooled_cv_errors

  names(mean_cv_errors) <- lambda_names

  mean_fold_cv_errors <- cpp_result$mean_fold_cv_errors

  names(mean_fold_cv_errors) <- lambda_names

  fold_errors <- cpp_result$fold_errors

  rownames(fold_errors) <- paste0("Fold_", seq_len(k))
  colnames(fold_errors) <- lambda_names

  coefficients <- cpp_result$coefficients

  predictor_names <- colnames(x)

  if (is.null(predictor_names)) {
    predictor_names <- paste0("x", seq_len(ncol(x)))
  }

  names(coefficients) <- c("(Intercept)", predictor_names)

  # ----------------------------------------------------------
  # 6. Construct result object
  # ----------------------------------------------------------

  result <- list(
    coefficients = coefficients,
    best_lambda = cpp_result$best_lambda,
    best_mse = cpp_result$best_mse,
    best_index = cpp_result$best_index,
    lambda_grid = lambda_grid,
    mean_cv_errors = mean_cv_errors,
    pooled_cv_errors = mean_cv_errors,
    mean_fold_cv_errors = mean_fold_cv_errors,
    fold_errors = fold_errors,
    fold_id = fold_id,
    k = k,
    seed = seed
  )

  class(result) <- "ridge_cv"

  result
}
