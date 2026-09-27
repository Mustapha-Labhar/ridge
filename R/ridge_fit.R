#' Fit a ridge regression model
#'
#' Fits a ridge regression model using a regularization parameter.
#'
#' @param x A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @param lambda A non-negative regularization parameter.
#'
#' @return A list containing the fitted coefficients and the value of
#'   the regularization parameter.
#'
#' @examples
#' x <- matrix(rnorm(20), ncol = 2)
#' y <- rnorm(10)
#' model <- ridge_fit(x, y, lambda = 1)
#' model
#'
#' @export
ridge_fit <- function(x, y, lambda = 1) {
  ridge(x, y, lambda)
}
