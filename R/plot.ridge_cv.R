#' Plot cross-validation errors for ridge regression
#'
#' Displays cross-validation errors as a function of
#' the regularization parameter.
#'
#' @param x An object of class "ridge_cv".
#' @param ... Additional graphical parameters.
#'
#' @method plot ridge_cv
#' @export
plot.ridge_cv <- function(x, ...) {

  # 1. Validate the input object

  if (!inherits(x, "ridge_cv")) {
    stop("x must be an object of class 'ridge_cv'.")
  }

  # 2. Extract the lambda grid

  lambda <- x$lambda_grid

  # 3. Extract cross-validation errors

  cv_errors <- x$fold_errors

  # 4. Calculate mean cross-validation errors

  mean_errors <- colMeans(cv_errors)

  # 5. Calculate standard errors

  sd_errors <- apply(cv_errors, 2, sd)

  se_errors <- sd_errors / sqrt(x$k)

  # 6. Identify lambda_min

  min_index <- which.min(mean_errors)

  lambda_min <- lambda[min_index]

  min_error <- mean_errors[min_index]

  se_min <- se_errors[min_index]

  # 7. Calculate the one-standard-error threshold

  threshold_1se <- min_error + se_min


  # 8. Identify lambda_1se

  eligible <- which(mean_errors <= threshold_1se)

  index_1se <- max(eligible)

  lambda_1se <- lambda[index_1se]

  # 9. Store the selected parameters

  selected_parameters <- c(
    lambda_min = unname(lambda_min),
    lambda_1se = unname(lambda_1se),
    min_error = unname(min_error),
    se_min = unname(se_min),
    threshold_1se = unname(threshold_1se)
  )

  # 10. Prepare the graphical display

  plot_args <- list(
    x = lambda,
    y = mean_errors,
    log = "x",
    type = "b",
    pch = 19,
    xlab = expression(lambda),
    ylab = "Mean cross-validation error",
    main = "Ridge regression: cross-validation"
  )

  plot_args <- modifyList(
    plot_args,
    list(...)
  )

  do.call(plot, plot_args)

  # 11. Add standard error bars

  arrows(
    x0 = lambda,
    y0 = mean_errors - se_errors,
    x1 = lambda,
    y1 = mean_errors + se_errors,
    angle = 90,
    code = 3,
    length = 0.05
  )

  # 12. Add lambda_min and lambda_1se reference lines

  abline(
    v = lambda_min,
    lty = 2,
    lwd = 2
  )

  abline(
    v = lambda_1se,
    lty = 3,
    lwd = 2
  )

  # 13. Add legend

  legend(
    "topleft",
    legend = c(
      "Mean CV error",
      "Standard error",
      expression(lambda[min]),
      expression(lambda[1-SE])
    ),
    lty = c(1, NA, 2, 3),
    pch = c(19, NA, NA, NA),
    lwd = c(1, 1, 2, 2),
    bty = "n"
  )

  invisible(selected_parameters)

}
