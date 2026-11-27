ridge – Ridge Regression Tools
================
Mustapha Labhar

- [Overview](#overview)
  - [Main features](#main-features)
  - [Installation](#installation)
  - [Ridge Regression](#ridge-regression)
    - [Example: fitting a model](#example-fitting-a-model)
  - [Cross-validation](#cross-validation)
    - [Cross-validation criteria](#cross-validation-criteria)
    - [Visualization](#visualization)
  - [Prediction](#prediction)
  - [Implementation](#implementation)
  - [Tests and quality control](#tests-and-quality-control)
  - [License](#license)

# Overview

`ridge` is an R package for fitting Ridge Regression models and
selecting regularization parameters through K-fold cross-validation.

The package combines R implementations with C++ routines using **Rcpp**
and **RcppArmadillo**.

## Main features

- Ridge Regression with an unpenalized intercept.
- K-fold cross-validation over a user-defined grid of strictly positive
  lambda values.
- Pooled cross-validation error and mean fold error calculations.
- Automatic selection of the regularization parameter.
- Visualization of cross-validation errors.
- A one-standard-error selection rule.
- S3 methods for fitted models and cross-validation results.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("Mustapha-Labhar/ridge")
```

Load the package:

``` r
library(ridge)
```

## Ridge Regression

Ridge Regression estimates the coefficients by minimizing:

$$
\widehat{\boldsymbol{\beta}}_\lambda =
\arg\min_{\beta_0,\boldsymbol{\beta}}
\left[
\sum_{i=1}^{n}
(y_i-\beta_0-\mathbf{x}_i^\top\boldsymbol{\beta})^2
+
\lambda\sum_{j=1}^{p}\beta_j^2
\right].
$$

The intercept is not penalized.

### Example: fitting a model

``` r
set.seed(123)

x <- matrix(rnorm(200), ncol = 2)
y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(100)

model <- ridge_fit(x, y, lambda = 1)

model
coef(model)
fitted(model)
residuals(model)
```

The fitted model has class `ridge_model` and supports the following S3
methods:

- `print()`
- `coef()`
- `fitted()`
- `residuals()`
- `predict()`

## Cross-validation

The `ridge_cv()` function performs K-fold cross-validation over a grid
of candidate regularization parameters.

``` r
lambda_grid <- 10^seq(-3, 2, length.out = 50)

cv_result <- ridge_cv(
  x,
  y,
  lambda_grid = lambda_grid,
  k = 5,
  seed = 123
)

cv_result$best_lambda
cv_result$best_mse
```

### Cross-validation criteria

The package calculates two complementary criteria.

The pooled cross-validation error is:

$$
CV_{\mathrm{pooled}}(\lambda)
=
\frac{1}{n}
\sum_{k=1}^{K}
\sum_{i\in V_k}
(y_i-\widehat y_i^{(-k)})^2.
$$

The mean fold cross-validation error is:

$$
CV_{\mathrm{fold}}(\lambda)
=
\frac{1}{K}
\sum_{k=1}^{K} E_k(\lambda),
$$

where $E_k(\lambda)$ is the mean squared prediction error in validation
fold $k$.

The `best_lambda` returned by `ridge_cv()` minimizes the pooled
cross-validation error.

### Visualization

``` r
plot(cv_result)
```

The plot provides two selection criteria:

- `lambda_min`: minimizes the mean fold cross-validation error.
- `lambda_1se`: selects the largest lambda whose mean fold error is
  within one estimated standard error of the minimum.

These criteria are based on the mean fold errors and may differ from
`best_lambda`, which uses the pooled criterion.

The plotting method invisibly returns the selected parameters:

``` r
selected_parameters <- plot(cv_result)

selected_parameters
```

## Prediction

New observations can be supplied to `predict()`:

``` r
new_data <- matrix(
  c(0.5, -0.2,
    1.0,  0.3),
  ncol = 2,
  byrow = TRUE
)

predict(model, newdata = new_data)
```

## Implementation

The package uses:

- **R** for the public interface and model methods.
- **C++** for computational routines.
- **Rcpp** and **RcppArmadillo** for the interface between R and C++ and
  matrix computations.

The intercept is excluded from the Ridge penalty.

## Tests and quality control

The package includes automated tests using `testthat`.

Run the tests with:

``` r
devtools::test()
```

Check the package with:

``` r
devtools::check()
```

## License

MIT License.
