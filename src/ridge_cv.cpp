
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector ridge_cv_cpp(
    NumericMatrix x,
    NumericVector y,
    NumericVector lambdas,
    IntegerVector fold_id,
    Function ridge_R
) {


  int n = x.nrow();
  int p = x.ncol();
  int K = max(fold_id);

  NumericVector mse(lambdas.size());

  for (int l = 0; l < lambdas.size(); l++) {

    double total_error = 0.0;

    for (int k = 1; k <= K; k++) {

      int n_train = 0;
      int n_valid = 0;

      for (int i = 0; i < n; i++) {
        if (fold_id[i] == k) {
          n_valid++;
        } else {
          n_train++;
        }
      }

      NumericMatrix x_train(n_train, p);
      NumericVector y_train(n_train);
      NumericMatrix x_valid(n_valid, p);
      NumericVector y_valid(n_valid);

      int a = 0;
      int b = 0;

      for (int i = 0; i < n; i++) {
        if (fold_id[i] == k) {
          y_valid[b] = y[i];

          for (int j = 0; j < p; j++) {
            x_valid(b, j) = x(i, j);
          }

          b++;
        } else {
          y_train[a] = y[i];

          for (int j = 0; j < p; j++) {
            x_train(a, j) = x(i, j);
          }

          a++;
        }
      }

      List fit = ridge_R(
        x_train,
        y_train,
        lambdas[l]
      );

      NumericVector coef = fit["coefficients"];

      for (int i = 0; i < n_valid; i++) {
        double prediction = coef[0];

        for (int j = 0; j < p; j++) {
          prediction += x_valid(i, j) * coef[j + 1];
        }

        double error = y_valid[i] - prediction;
        total_error += error * error;
      }
    }

    mse[l] = total_error / n;
  }

  return mse;
}
