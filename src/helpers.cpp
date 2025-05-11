#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <cmath>

// Function to round a number to a given number of decimal places
double roundTo(double value, int decimals) {
  double factor = std::pow(10.0, decimals);
  return std::round(value * factor) / factor;
}

// [[Rcpp::export]]
arma::vec candidate_cor_cpp(const arma::mat &candidate, const arma::vec &outcome) {
  // Combine candidate predictors and outcome column
  arma::mat full_data = arma::join_horiz(candidate, outcome);

  // Compute the correlation matrix
  arma::mat C = arma::cor(full_data);

  // Use number of columns since the matrix is square.
  int p = C.n_cols;
  int num_elements = (p * (p - 1)) / 2;
  arma::vec cor_vec(num_elements);
  int idx = 0;

  // Extract upper-triangular elements in column-major order:
  for (int j = 1; j < p; j++) {
    for (int i = 0; i < j; i++) {
      cor_vec(idx++) = C(i, j);
    }
  }
  return cor_vec;
}

// [[Rcpp::export]]
arma::vec candidate_reg_cpp(const arma::mat &candidate,
                            const arma::vec &y,
                            const arma::uvec &positions) {
  // candidate: matrix of predictors (without intercept)
  // y: outcome vector
  // positions: a vector (1-indexed, as produced by R's match()) indicating
  // which columns of the full design matrix to use for the regression.

  int n = candidate.n_rows;
  int p = candidate.n_cols;
  int num_interactions = (p * (p - 1)) / 2;
  int total_cols = 1 + p + num_interactions;  // intercept + main effects + interactions

  // Build full design matrix X.
  arma::mat X(n, total_cols, arma::fill::ones);
  // Main effects: columns 1 to p (column 0 is intercept).
  X.cols(1, p) = candidate;

  // Fill in interaction terms in the same order as in your R function:
  int col_index = p + 1;
  for (int i = 0; i < p; i++) {
    for (int j = i + 1; j < p; j++) {
      X.col(col_index) = candidate.col(i) % candidate.col(j);
      col_index++;
    }
  }

  // Now, subset the full design matrix using the positions vector.
  // Note: positions are 1-indexed in R; convert to 0-indexed in C++.
  arma::mat X_sub(n, positions.n_elem);
  for (arma::uword i = 0; i < positions.n_elem; i++) {
    // Subtract 1 to convert from R's 1-indexing to C++'s 0-indexing.
    unsigned int pos = positions(i) - 1;
    X_sub.col(i) = X.col(pos);
  }

  // Solve the OLS problem using only the columns specified by X_sub.
  arma::vec beta = arma::solve(X_sub, y);
  return beta;
}

// [[Rcpp::export]]
arma::mat candidate_reg_cpp_se(const arma::mat &candidate,
                            const arma::vec &y,
                            const arma::uvec &positions) {
  // candidate: matrix of predictors (without intercept)
  // y: outcome vector
  // positions: a vector (1-indexed, as produced by R's match()) indicating
  // which columns of the full design matrix to use for the regression.

  int n = candidate.n_rows;
  int p = candidate.n_cols;
  int num_interactions = (p * (p - 1)) / 2;
  int total_cols = 1 + p + num_interactions;  // intercept + main effects + interactions

  // Build full design matrix X.
  arma::mat X(n, total_cols, arma::fill::ones);

  // Main effects: columns 1 to p (column 0 is intercept).
  X.cols(1, p) = candidate;

  // Fill in interaction terms in the same order as in your R function:
  int col_index = p + 1;
  for (int i = 0; i < p; i++) {
    for (int j = i + 1; j < p; j++) {
      X.col(col_index) = candidate.col(i) % candidate.col(j);
      col_index++;
    }
  }

  // Subset the design matrix using the positions vector.
  // Note: positions are 1-indexed in R; convert to 0-indexed in C++.
  arma::mat X_sub(n, positions.n_elem);
  for (arma::uword i = 0; i < positions.n_elem; i++) {
    unsigned int pos = positions(i) - 1;
    if (pos >= X.n_cols) {
      Rcpp::stop("Error: a position is out of range.");
    }
    X_sub.col(i) = X.col(pos);
  }

  // Solve the OLS problem using only the columns specified by X_sub.
  arma::vec beta = arma::solve(X_sub, y);

  // Calculate residuals.
  arma::vec residuals = y - X_sub * beta;

  // Degrees of freedom: n - k, where k is number of parameters estimated.
  int k = X_sub.n_cols;
  double sigma2_hat = arma::dot(residuals, residuals) / (n - k);

  // Variance-covariance matrix of beta estimates.
  arma::mat XtX_inv = arma::inv_sympd(X_sub.t() * X_sub);
  arma::mat var_beta = sigma2_hat * XtX_inv;

  // Standard errors: square roots of the diagonal elements.
  arma::vec se = arma::sqrt(var_beta.diag());

  // Combine beta and se into a matrix: first column beta, second column se.
  arma::mat out(beta.n_elem, 2);
  out.col(0) = beta;
  out.col(1) = se;

  return out;
}


// Helper: Objective function for the integer vector optimization
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double objective_cpp(NumericVector x,
                     double target_mean,
                     double target_sd,
                     NumericVector obj_weight,
                     double eps,
                     int mean_dec,
                     int sd_dec) {
  int n = x.size();
  if(n < 2) return NA_REAL; // Avoid division by zero in sd calculation

  // Compute mean of x
  double sum = 0.0;
  for (int i = 0; i < n; i++) {
    sum += x[i];
  }
  double mean_x = sum / n;

  // Compute sample standard deviation (denom = n - 1)
  double ssd = 0.0;
  for (int i = 0; i < n; i++) {
    double diff = x[i] - mean_x;
    ssd += diff * diff;
  }
  double sd_x = std::sqrt(ssd / (n - 1));

  // Use eps to avoid dividing by a very small number.
  double denom_mean = std::max(std::fabs(target_mean), eps);
  double denom_sd   = std::max(std::fabs(target_sd), eps);

  // Compute squared relative errors (using inline multiplication for speed).
  double diff_mean = (std::round(mean_x * std::pow(10.0, mean_dec)) / std::pow(10.0, mean_dec)) - target_mean;
  double diff_sd   = (std::round(sd_x   * std::pow(10.0, sd_dec)) / std::pow(10.0, sd_dec)) - target_sd;

  double mean_error = std::sqrt((diff_mean / denom_mean) * (diff_mean / denom_mean));
  double sd_error   = std::sqrt((diff_sd / denom_sd) * (diff_sd / denom_sd));

  // Combine errors using the provided weights.
  double total_error = mean_error * obj_weight[0] + sd_error * obj_weight[1];

  return total_error;
}


#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List error_function_cpp(const arma::mat &candidate,
                              const arma::vec &outcome,
                              const arma::vec &target_cor,
                              const arma::vec &target_reg,
                              const arma::vec &weight,
                              const arma::uvec &positions,
                              const double cor_dec,
                              const double reg_dec) {
  // Compute candidate's correlation vector.
  arma::vec cor_vec = candidate_cor_cpp(candidate, outcome);

  // Round each element of the correlation vector using a single decimals value.
  double factor_cor = std::pow(10.0, cor_dec);
  for (arma::uword i = 0; i < cor_vec.n_elem; i++) {
    cor_vec(i) = std::round(cor_vec(i) * factor_cor) / factor_cor;
  }

  // Calculate the RMSE for correlations.
  arma::uvec idx = arma::find_finite(target_cor);
  double cor_error = std::sqrt( arma::accu( arma::square(cor_vec.elem(idx) - target_cor.elem(idx)) ) / idx.n_elem );

  // Compute candidate's regression coefficients.
  arma::vec reg_vec = candidate_reg_cpp(candidate, outcome, positions);

  // Round each element of the regression vector using a single decimals value.
  double factor_reg = std::pow(10.0, reg_dec);
  for (arma::uword i = 0; i < reg_vec.n_elem; i++) {
    reg_vec(i) = std::round(reg_vec(i) * factor_reg) / factor_reg;
  }

  // Calculate the RMSE for regression coefficients.
  arma::uvec idxx = arma::find_finite(target_reg);
  double reg_error = std::sqrt( arma::accu( arma::square(reg_vec.elem(idxx) - target_reg.elem(idxx)) ) / idxx.n_elem );

  // Compute the total weighted error.
  double total_error = cor_error * weight(0) + reg_error * weight(1);

  // Compute the error ratio (with protection against division by zero).
  double error_ratio = (reg_error == 0.0) ? R_PosInf : cor_error / reg_error;

  return Rcpp::List::create(Rcpp::Named("total_error") = total_error,
                            Rcpp::Named("error_ratio") = error_ratio);
}



// [[Rcpp::export]]
Rcpp::List error_function_cpp_se(const arma::mat &candidate,
                                 const arma::vec &outcome,
                                 const arma::vec &target_cor,
                                 const arma::mat &target_reg_se,
                                 const arma::vec &weight,
                                 const arma::uvec &positions,
                                 const double cor_dec,
                                 const double reg_dec) {
  // Compute candidate's correlation vector.
  arma::vec cor_vec = candidate_cor_cpp(candidate, outcome);

  // Round each element of the correlation vector.
  double factor_cor = std::pow(10.0, cor_dec);
  for (arma::uword i = 0; i < cor_vec.n_elem; i++) {
    cor_vec(i) = std::round(cor_vec(i) * factor_cor) / factor_cor;
  }

  // Calculate the RMSE for correlations.
  arma::uvec idx = arma::find_finite(target_cor);
  double cor_error = std::sqrt( arma::accu( arma::square(cor_vec.elem(idx) - target_cor.elem(idx)) ) / idx.n_elem );

  // Compute candidate's regression coefficients' standard error.
  arma::mat reg_se = candidate_reg_cpp_se(candidate, outcome, positions);

  // Round each element of the regression SE matrix.
  double factor_reg = std::pow(10.0, reg_dec);
  for (arma::uword i = 0; i < reg_se.n_elem; i++) {
    reg_se(i) = std::round(reg_se(i) * factor_reg) / factor_reg;
  }

  // Calculate the RMSE for regression standard errors.
  arma::uvec idxx = arma::find_finite(target_reg_se);
  double reg_error = std::sqrt( arma::accu( arma::square(reg_se.elem(idxx) - target_reg_se.elem(idxx)) ) / idxx.n_elem );

  // Compute the total weighted error.
  double total_error = cor_error * weight(0) + reg_error * weight(1);

  // Compute the error ratio (with protection against division by zero).
  double error_ratio = (reg_error == 0.0) ? R_PosInf : cor_error / reg_error;

  return Rcpp::List::create(Rcpp::Named("total_error") = total_error,
                            Rcpp::Named("error_ratio") = error_ratio);
}




// Helper: OLS soultion given design matrix and outcome
// [[Rcpp::export]]
arma::vec ols_from_design(const arma::mat &X, const arma::vec &y) {
  // X: design matrix from R's model.matrix() (including intercept and any interactions)
  // y: outcome vector

  // Solve the OLS problem: find beta such that X * beta approximates y.
  arma::vec beta = arma::solve(X, y);

  // Return the estimated coefficients as a plain vector.
  return beta;
}
