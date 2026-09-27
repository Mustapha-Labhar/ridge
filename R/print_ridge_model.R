#' Print a ridge regression model
#'
#' @param x A fitted ridge regression model.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the model.
#' @export
print.ridge_model <- function(x, ...) {
  cat("Ridge Regression Model\n")
  cat("----------------------\n")
  cat("Lambda:", x$lambda, "\n\n")

  cat("Coefficients:\n")
  print(x$coefficients)

  invisible(x)
}
