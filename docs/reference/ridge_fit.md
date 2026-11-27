# Fit a ridge regression model

Fits a ridge regression model using a regularization parameter.

## Usage

``` r
ridge_fit(x, y, lambda = 1)
```

## Arguments

- x:

  A numeric matrix of predictors.

- y:

  A numeric vector of responses.

- lambda:

  A non-negative regularization parameter.

## Value

A list containing the fitted coefficients and the value of the
regularization parameter.

## Examples

``` r
x <- matrix(rnorm(20), ncol = 2)
y <- rnorm(10)
model <- ridge_fit(x, y, lambda = 1)
model
#> Ridge Regression Model
#> ----------------------
#> Lambda: 1 
#> 
#> Coefficients:
#> (Intercept)          x1          x2 
#>  0.43820419  0.39254017  0.04039178 
```
