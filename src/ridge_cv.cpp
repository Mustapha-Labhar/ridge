// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_USE_BLAS
#define ARMA_USE_LAPACK
#include <RcppArmadillo.h>

using namespace Rcpp;


// ------------------------------------------------------------
// Internal function: fit a ridge regression model in C++
// The intercept is not penalized.
// ------------------------------------------------------------

arma::vec ridge_coefficients_cpp(
    const arma::mat& x,
    const arma::vec& y,
    const double lambda
) {

  const arma::uword n = x.n_rows;
  const arma::uword p = x.n_cols;

  // Add an intercept column
  arma::mat X_aug(n, p + 1, arma::fill::ones);

  if (p > 0) {
    X_aug.cols(1, p) = x;
  }

  // Ridge normal equations
  arma::mat A = X_aug.t() * X_aug;

  // Penalize predictors, but not the intercept
  for (arma::uword j = 1; j <= p; j++) {
    A(j, j) += lambda;
  }

  arma::vec b = X_aug.t() * y;

  // Solve the linear system without explicitly computing
  // the inverse of A
  arma::vec beta;

  bool success = arma::solve(
    beta,
    A,
    b,
    arma::solve_opts::likely_sympd
  );

  if (!success) {
    success = arma::solve(beta, A, b);
  }

  if (!success || !beta.is_finite()) {
    stop("Numerical failure while fitting the ridge model.");
  }

  return beta;
}


// ------------------------------------------------------------
// Main function: ridge cross-validation entirely in C++
// ------------------------------------------------------------

// [[Rcpp::export]]
List ridge_cv_cpp(
    NumericMatrix x,
    NumericVector y,
    NumericVector lambdas,
    IntegerVector fold_id
) {

  const int n = x.nrow();
  const int p = x.ncol();

  // Basic input validation

  if (n < 2 || p < 1) {
    stop("x must contain at least two observations and one predictor.");
  }

  if (y.size() != n) {
    stop("y must have the same number of observations as x.");
  }

  if (fold_id.size() != n) {
    stop("fold_id must have the same length as the number of observations.");
  }

  if (lambdas.size() == 0) {
    stop("lambdas must contain at least one value.");
  }

  for (int i = 0; i < n; i++) {
    if (!std::isfinite(y[i])) {
      stop("y must contain only finite values.");
    }

    for (int j = 0; j < p; j++) {
      if (!std::isfinite(x(i, j))) {
        stop("x must contain only finite values.");
      }
    }
  }

  for (int l = 0; l < lambdas.size(); l++) {
    if (!std::isfinite(lambdas[l]) || lambdas[l] <= 0) {
      stop("All lambdas must be strictly positive and finite.");
    }
  }

  // Validate fold assignments



  int K = 0;

  for (int i = 0; i < n; i++) {

    if (fold_id[i] < 1) {
      stop("Fold identifiers must be positive integers.");
    }

    if (fold_id[i] > n) {
      stop("Fold identifiers cannot exceed the number of observations.");
    }

    K = std::max(K, fold_id[i]);
  }

  if (K < 2) {
    stop("At least two folds are required.");
  }

  IntegerVector fold_counts(K);

  // Count observations in each fold
  for (int i = 0; i < n; i++) {
    fold_counts[fold_id[i] - 1]++;
  }

  // Check that fold identifiers are consecutive
  for (int k = 0; k < K; k++) {
    if (fold_counts[k] == 0) {
      stop("Fold identifiers must be consecutive integers starting at 1.");
    }
  }

  // Convert inputs to Armadillo objects
  arma::mat X = as<arma::mat>(x);
  arma::vec Y = as<arma::vec>(y);

  // Results

  NumericMatrix fold_errors(K, lambdas.size());

  NumericVector pooled_cv_errors(lambdas.size());
  NumericVector mean_fold_cv_errors(lambdas.size());

  // Cross-validation

  for (int l = 0; l < lambdas.size(); l++) {

    double total_error = 0.0;
    double sum_fold_mse = 0.0;

    for (int k = 1; k <= K; k++) {

      const int n_valid = fold_counts[k - 1];
      const int n_train = n - n_valid;

      arma::mat X_train(n_train, p);
      arma::vec Y_train(n_train);

      int train_index = 0;

      for (int i = 0; i < n; i++) {
        if (fold_id[i] != k) {

          X_train.row(train_index) = X.row(i);
          Y_train[train_index] = Y[i];

          train_index++;
        }
      }

      // Fit ridge regression directly in C++

      arma::vec beta = ridge_coefficients_cpp(
        X_train,
        Y_train,
        lambdas[l]
      );

      // Validation predictions and squared errors

      double fold_error = 0.0;

      for (int i = 0; i < n; i++) {

        if (fold_id[i] == k) {

          double prediction = beta[0];

          for (int j = 0; j < p; j++) {
            prediction += X(i, j) * beta[j + 1];
          }

          const double error = Y[i] - prediction;

          fold_error += error * error;
        }
      }

      const double fold_mse = fold_error / n_valid;

      fold_errors(k - 1, l) = fold_mse;

      total_error += fold_error;
      sum_fold_mse += fold_mse;
    }

    // Two possible aggregations of fold errors

    pooled_cv_errors[l] = total_error / n;

    mean_fold_cv_errors[l] = sum_fold_mse / K;
  }

  // Select the best lambda using pooled CV error

  const int best_index = which_min(pooled_cv_errors);

  const double best_lambda = lambdas[best_index];

  const double best_mse = pooled_cv_errors[best_index];

  // Fit the final model on all observations

  arma::vec final_beta = ridge_coefficients_cpp(
    X,
    Y,
    best_lambda
  );

  return List::create(
    Named("lambda_grid") = lambdas,
    Named("fold_errors") = fold_errors,
    Named("pooled_cv_errors") = pooled_cv_errors,
    Named("mean_fold_cv_errors") = mean_fold_cv_errors,
    Named("best_lambda") = best_lambda,
    Named("best_mse") = best_mse,
    Named("best_index") = best_index + 1,
    Named("coefficients") = wrap(final_beta)
  );
}
