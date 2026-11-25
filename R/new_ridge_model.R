
new_ridge_model <- function(
  coefficients,
  lambda,
  fitted_values,
  residuals
) {

  # 1. Validate coefficients

  if (!is.numeric(coefficients) ||
      is.complex(coefficients) ||
      !is.null(dim(coefficients)) ||
      length(coefficients) < 2L ||
      any(!is.finite(coefficients))) {
    stop("coefficients must be a finite numeric vector.")
  }

  if (is.null(names(coefficients)) ||
      anyNA(names(coefficients)) ||
      any(names(coefficients) == "") ||
      anyDuplicated(names(coefficients))) {
    stop("coefficients must have unique, non-empty names.")
  }

  # 2. Validate lambda

  if (length(lambda) != 1L ||
      !is.numeric(lambda) ||
      is.complex(lambda) ||
      !is.finite(lambda) ||
      lambda < 0) {
    stop("lambda must be a single non-negative numeric value.")
  }

  # 3. Validate fitted values
  if (!is.numeric(fitted_values) ||
      is.complex(fitted_values) ||
      !is.null(dim(fitted_values)) ||
      length(fitted_values) < 1L ||
      any(!is.finite(fitted_values))) {
    stop("fitted_values must be a non-empty finite numeric vector.")
  }

  # 4. Validate residuals
  if (!is.numeric(residuals) ||
      is.complex(residuals) ||
      !is.null(dim(residuals)) ||
      length(residuals) < 1L ||
      any(!is.finite(residuals))) {
    stop("residuals must be a non-empty finite numeric vector.")
  }

  # 5. Check consistency

  if (length(fitted_values) != length(residuals)) {
    stop("fitted_values and residuals must have the same length.")
  }

  # 6. Construct the S3 object

  structure(
    list(
      coefficients = coefficients,
      lambda = lambda,
      fitted.values = fitted_values,
      residuals = residuals
    ),
    class = "ridge_model"
  )
}
