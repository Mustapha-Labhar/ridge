
#' Predict from a Ridge Regression Model
#'
#' Computes predictions using a fitted ridge_model object.
#'
#' @param object An object of class ridge_model.
#' @param newdata A numeric matrix or data frame containing new predictors.
#' @param ... Additional arguments.
#'
#' @return A numeric vector of predicted values.
#'
#' @export
predict.ridge_model <- function(object, newdata, ...) {

  # 1. Validate model object
  if (!inherits(object, "ridge_model")) {
    stop("object must inherit from class ridge_model.")
  }

  # 2. Validate newdata
  if (missing(newdata)) {
    stop("newdata must be supplied.")
  }

  if (!is.matrix(newdata)) {
    newdata <- as.matrix(newdata)
  }

  if (!is.numeric(newdata) || is.complex(newdata)) {
    stop("newdata must be numeric.")
  }

  if (length(dim(newdata)) != 2L ||
      nrow(newdata) < 1L ||
      ncol(newdata) < 1L) {
    stop("newdata must contain at least one observation and one predictor.")
  }

  if (any(!is.finite(newdata))) {
    stop("newdata must contain only finite values.")
  }

  # 3. Extract model coefficients
  beta <- coef(object)

  predictor_names <- names(beta)[-1L]

  # 4. Validate number of predictors
  if (ncol(newdata) != length(predictor_names)) {
    stop("newdata must have the same number of predictors as the model.")
  }

  # 5. Validate and align predictor names
  new_names <- colnames(newdata)

  if (!is.null(new_names)) {

    if (anyDuplicated(new_names)) {
      stop("newdata must not contain duplicated column names.")
    }

    if (!setequal(new_names, predictor_names)) {
      stop("newdata column names must match the model predictors.")
    }

    newdata <- newdata[, predictor_names, drop = FALSE]
  }

  # 6. Compute predictions
  as.vector(
    beta[1L] + newdata %*% beta[-1L]
  )
}
