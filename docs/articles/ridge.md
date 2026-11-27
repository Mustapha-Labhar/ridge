# Ridge Regression with ridge

## Introduction

The `ridge` package provides tools for fitting and working with ridge
regression models. It includes model fitting, cross-validation for
selecting the regularization parameter, graphical visualization of
cross-validation errors, and prediction methods.

This vignette presents a complete and reproducible workflow using the
main functions and S3 methods implemented in the package.

## The Ridge Regression Model

Ridge regression is a regularized linear regression method designed to
reduce the magnitude of regression coefficients.

The estimator minimizes the following penalized least-squares criterion:

``` math

\widehat{\beta}_{\lambda}
=
\arg\min_{\beta}
\left[
\sum_{i=1}^{n}
(y_i-\beta_0-x_i^\top\beta)^2
+
\lambda\sum_{j=1}^{p}\beta_j^2
\right].
```

Here, $`\lambda`$ controls the strength of regularization. A larger
value of $`\lambda`$ produces stronger shrinkage of the
explanatory-variable coefficients.

The intercept is not penalized in the implementation used by this
package.

## Generating a Reproducible Dataset

We generate a simulated dataset with two explanatory variables and a
known underlying linear relationship.

``` r
set.seed(123)

x <- matrix(rnorm(200), ncol = 2)
colnames(x) <- c("x1", "x2")

y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)
```

The data-generating process is:

``` math

Y_i = 2 + 3X_{i1} - X_{i2} + \varepsilon_i,
```

where the error term follows a standard normal distribution.

The theoretical coefficients are therefore 2 for the intercept, 3 for
the first explanatory variable, and -1 for the second.

## Fitting a Ridge Model

The
[`ridge_fit()`](https://mustapha-labhar.github.io/ridge/reference/ridge_fit.md)
function fits a ridge regression model for a specified regularization
parameter.

``` r
library(ridge)

model <- ridge_fit(
  x = x,
  y = y,
  lambda = 1
)

model
#> Ridge Regression Model
#> ----------------------
#> Lambda: 1 
#> 
#> Coefficients:
#> (Intercept)          x1          x2 
#>   2.1390816   2.8329507  -0.9673232
coef(model)
#> (Intercept)          x1          x2 
#>   2.1390816   2.8329507  -0.9673232
```

The resulting object belongs to the S3 class `ridge_model`.

The estimated coefficients can be compared with the theoretical values
used to generate the data.

``` r
head(fitted(model))
#> [1] 1.238475 1.238511 6.793456 2.675014 3.425870 7.041333
head(residuals(model))
#> [1]  1.9893154  1.1264862 -0.1357845  0.4272474 -0.5007284 -0.3273568
```

## Cross-Validation

The
[`ridge_cv()`](https://mustapha-labhar.github.io/ridge/reference/ridge_cv.md)
function performs k-fold cross-validation over a user-defined grid of
regularization parameters.

We consider 50 values of lambda distributed on a logarithmic scale.

``` r
lambda_grid <- 10^seq(-3, 2, length.out = 50)

cv_result <- ridge_cv(
  x = x,
  y = y,
  lambda_grid = lambda_grid,
  k = 5,
  seed = 123
)

cv_result$best_lambda
#> [1] 0.08685114
cv_result$best_mse
#> [1] 0.9270251
```

The selected parameter minimizes the estimated mean cross-validation
error over the specified grid.

## Visualizing Cross-Validation Errors

The S3 plotting method displays the mean cross-validation error as a
function of lambda, together with its standard error and the selected
regularization parameters.

``` r
selected_parameters <- plot(cv_result)
```

![](ridge_files/figure-html/cross-validation-plot-1.png)

``` r

selected_parameters
#>    lambda_min    lambda_1se     min_error        se_min threshold_1se 
#>    0.08685114    7.54312006    0.92702508    0.08993552    1.01696061
```

Two selection criteria are considered:

- The minimum-error criterion selects the lambda associated with the
  smallest estimated cross-validation error.
- The one-standard-error rule selects the largest lambda whose estimated
  error remains within one standard error of the minimum.

The latter favors stronger regularization while accepting a
cross-validation error close to the minimum.

## Comparing the Selected Models

We fit two models using the parameters selected by the two criteria.

``` r
model_min <- ridge_fit(
  x = x,
  y = y,
  lambda = selected_parameters["lambda_min"]
)

model_1se <- ridge_fit(
  x = x,
  y = y,
  lambda = selected_parameters["lambda_1se"]
)

comparison <- cbind(
  Theoretical = c(2, 3, -1),
  Lambda_min = coef(model_min),
  Lambda_1se = coef(model_1se)
)

comparison
#>             Theoretical Lambda_min Lambda_1se
#> (Intercept)           2  2.1354178  2.1633122
#> x1                    3  2.8638539  2.6296842
#> x2                   -1 -0.9754127 -0.9128905
```

The Euclidean norms of the explanatory-variable coefficients can be used
to illustrate the shrinkage induced by regularization.

``` r
beta_min <- coef(model_min)[-1]
beta_1se <- coef(model_1se)[-1]

sqrt(sum(beta_min^2))
#> [1] 3.025407
sqrt(sum(beta_1se^2))
#> [1] 2.783632
```

## Prediction

The [`predict()`](https://rdrr.io/r/stats/predict.html) method allows
predictions to be obtained for new observations.

``` r
newdata <- x[1:5, , drop = FALSE]

predict(model, newdata = newdata)
#> [1] 1.238475 1.238511 6.793456 2.675014 3.425870
```

We can also compare the training mean squared errors of the two selected
models.

``` r
pred_min <- predict(model_min, newdata = x)
pred_1se <- predict(model_1se, newdata = x)

mse_min <- mean((y - pred_min)^2)
mse_1se <- mean((y - pred_1se)^2)

data.frame(
  Model = c("Minimum CV", "One-standard-error rule"),
  Lambda = c(
    selected_parameters["lambda_min"],
    selected_parameters["lambda_1se"]
  ),
  Training_MSE = c(mse_min, mse_1se)
)
#>                              Model     Lambda Training_MSE
#> lambda_min              Minimum CV 0.08685114    0.8778267
#> lambda_1se One-standard-error rule 7.54312006    0.9292174
```

These training errors describe the fit on the observations used for
model estimation. They should not be interpreted as independent
estimates of predictive performance.

## Conclusion

The `ridge` package provides a workflow for fitting ridge regression
models, selecting regularization parameters through cross-validation,
visualizing validation errors, and generating predictions.

The minimum-error and one-standard-error criteria provide different
regularization choices. Their comparison illustrates the relationship
between coefficient shrinkage and model fit.

The examples in this vignette are reproducible and use a simulated
dataset with known theoretical coefficients.
