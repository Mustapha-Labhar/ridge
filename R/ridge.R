ridge <- function(x, y, lambda = 1) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (!is.numeric(y)) {
    stop("y must be numeric.")
  }

  if (length(y) != nrow(x)) {
    stop("y must have the same number of observations as x.")
  }

  if (length(lambda) != 1 || !is.numeric(lambda) ||
      is.na(lambda) || lambda < 0) {
    stop("lambda must be a single non-negative numeric value.")
  }

  predictor_names <- colnames(x)

  if (is.null(predictor_names)) {
    predictor_names <- paste0("x", seq_len(ncol(x)))
  }

  x <- cbind(1, x)
  colnames(x) <- c("(Intercept)", predictor_names)

  p <- ncol(x)

  penalty <- diag(p)
  penalty[1, 1] <- 0

  coefficients <- solve(
    crossprod(x) + lambda * penalty,
    crossprod(x, y)
  )

  names(coefficients) <- colnames(x)

  result <- list(
    coefficients = as.vector(coefficients),
    lambda = lambda
  )

  names(result$coefficients) <- c("(Intercept)", predictor_names)

  class(result) <- "ridge_model"

  result
}
