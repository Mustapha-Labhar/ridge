# Select the ridge regularization parameter using cross-validation

Selects the regularization parameter using k-fold cross-validation
performed by the C++ implementation.

## Usage

``` r
ridge_cv(x, y, lambda_grid, k = 5, seed = 123)
```

## Arguments

- x:

  A numeric matrix of predictors.

- y:

  A numeric vector of responses.

- lambda_grid:

  A numeric vector of strictly positive regularization parameters to
  evaluate.

- k:

  Number of folds for cross-validation.

- seed:

  Random seed for reproducibility.

## Value

An object of class "ridge_cv" containing the selected lambda,
cross-validation errors, fold-specific errors, coefficients, and fold
assignments.

## Examples

``` r
x <- matrix(rnorm(80), ncol = 2)
y <- 2 + 3 * x[, 1] - x[, 2] + rnorm(40)
result <- ridge_cv(x, y, 10^seq(-3, 2, length.out = 10))
result$best_lambda
#> [1] 0.001
```
