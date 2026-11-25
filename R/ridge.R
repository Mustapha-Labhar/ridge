
ridge <- function(x, y, lambda = 1) {

  # ----------------------------------------------------------
  # 1. Validate predictors
  # ----------------------------------------------------------

  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  if (!is.numeric(x) || is.complex(x)) {
    stop("x must be numeric.")
  }

  if (length(dim(x)) != 2L ||
      nrow(x) < 1L ||
      ncol(x) < 1L) {
    stop("x must be a numeric matrix with predictors.")
  }

  if (any(!is.finite(x))) {
    stop("x must contain only finite values.")
  }

  # ----------------------------------------------------------
  # 2. Validate response
  # ----------------------------------------------------------

  if (!is.numeric(y) || is.complex(y)) {
    stop("y must be numeric.")
  }

  if (!is.null(dim(y))) {
    stop("y must be a numeric vector.")
  }

  if (any(!is.finite(y))) {
    stop("y must contain only finite values.")
  }

  if (length(y) != nrow(x)) {
    stop("y must have the same number of observations as x.")
  }

  # ----------------------------------------------------------
  # 3. Validate regularization parameter
  # ----------------------------------------------------------

  if (length(lambda) != 1L ||
      !is.numeric(lambda) ||
      is.complex(lambda) ||
      !is.finite(lambda) ||
      lambda < 0) {
    stop("lambda must be a single non-negative numeric value.")
  }

  # ----------------------------------------------------------
  # 4. Prepare predictor names
  # ----------------------------------------------------------

  predictor_names <- colnames(x)

  if (is.null(predictor_names)) {
    predictor_names <- paste0("x", seq_len(ncol(x)))
  }

  # ----------------------------------------------------------
  # 5. Construct design matrix
  # ----------------------------------------------------------

  x <- cbind(1, x)
  colnames(x) <- c("(Intercept)", predictor_names)

  p <- ncol(x)

  # ----------------------------------------------------------
  # 6. Construct penalty matrix
  #    Intercept remains unpenalized
  # ----------------------------------------------------------

  penalty <- diag(p)
  penalty[1, 1] <- 0

  # ----------------------------------------------------------
  # 7. Estimate Ridge coefficients
  # ----------------------------------------------------------

  coefficients <- solve(
    crossprod(x) + lambda * penalty,
    crossprod(x, y)
  )

  names(coefficients) <- colnames(x)

  # ----------------------------------------------------------
  # 8. Calculate fitted values and residuals
  # ----------------------------------------------------------

  fitted_values <- as.vector(x %*% coefficients)

  residual_values <- as.vector(y - fitted_values)

  # ----------------------------------------------------------
  # 9. Construct result
  # ----------------------------------------------------------

  # 9. Construct ridge model
  coefficients <- as.vector(coefficients)

  names(coefficients) <- c(
    "(Intercept)",
    predictor_names
  )

  new_ridge_model(
    coefficients = coefficients,
    lambda = lambda,
    fitted_values = fitted_values,
    residuals = residual_values
  )
}
