
#' Select the ridge regularization parameter using cross-validation
#'
#' @param x A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @param lambda_grid A numeric vector of strictly positive
#'   regularization parameters to evaluate.
#' @param k Number of folds for cross-validation.
#' @param seed Random seed for reproducibility.
#'
#' @return A list containing the selected lambda, the mean
#'   cross-validation errors, the minimum error, and fold assignments.
#'
#' @export
ridge_cv <- function(x, y, lambda_grid, k = 5, seed = 123) {

  # Argument validation

  x <- as.matrix(x)

  if (!is.numeric(x) || length(dim(x)) != 2L ||
      ncol(x) < 1L || any(!is.finite(x))) {
    stop("x must be a finite numeric matrix with predictors.")
  }

  if (!is.numeric(y) || !is.null(dim(y)) ||
      any(!is.finite(y))) {
    stop("y must be a finite numeric vector.")
  }

  if (length(y) != nrow(x)) {
    stop("y must have the same number of observations as x.")
  }

  if (length(k) != 1L || !is.numeric(k) ||
      !is.finite(k) || k != floor(k) ||
      k < 2L || k > nrow(x)) {
    stop("k must be an integer between 2 and nrow(x).")
  }

  if (!is.numeric(lambda_grid) ||
      length(lambda_grid) == 0L ||
      any(!is.finite(lambda_grid)) ||
      any(lambda_grid <= 0)) {
    stop("lambda_grid must contain strictly positive finite values.")
  }

  if (length(seed) != 1L || !is.numeric(seed) ||
      !is.finite(seed) || seed != floor(seed) ||
      abs(seed) > .Machine$integer.max) {
    stop("seed must be a valid integer.")
  }

  # Ensure double-precision numeric data for Rcpp

  storage.mode(x) <- "double"
  y <- as.numeric(y)

  # Create cross-validation folds without changing
  # the user's existing random-number state

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
    } else if (exists(".Random.seed",
                      envir = .GlobalEnv,
                      inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))

  n <- nrow(x)
  fold_id <- sample(rep(seq_len(k), length.out = n))

  # Calculate cross-validation errors using C++

  mse <- ridge_cv_cpp(
    x = x,
    y = y,
    lambdas = lambda_grid,
    fold_id = fold_id,
    ridge_R = ridge
  )

  names(mse) <- paste0(
    "lambda_",
    format(lambda_grid, scientific = TRUE)
  )

  # Select the best lambda

  best_index <- which.min(mse)

  result <- list(
    best_lambda = lambda_grid[best_index],
    best_mse = mse[best_index],
    mean_cv_errors = mse,
    lambda_grid = lambda_grid,
    fold_id = fold_id
  )

  class(result) <- "ridge_cv"

  result
}
