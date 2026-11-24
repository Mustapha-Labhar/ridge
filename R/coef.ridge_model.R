 #' Extract Ridge Model Coefficients
#'
#' Extracts the estimated coefficients from a ridge_model object.
#'
#' @param object An object of class ridge_model.
#' @param ... Additional arguments.
#'
#' @return A named numeric vector containing the estimated coefficients.
#' @importFrom stats coef
#' @export
coef.ridge_model <- function(object, ...) {

  object$coefficients

}
