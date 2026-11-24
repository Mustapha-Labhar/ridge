
#' Extract Residuals from a Ridge Model
#'
#' Extracts the residuals from a ridge_model object.
#'
#' @param object An object of class ridge_model.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing the residuals.
#'
#' @export
residuals.ridge_model <- function(object, ...) {

  if (!inherits(object, "ridge_model")) {
    stop("object must inherit from class ridge_model.")
  }

  object$residuals
}
