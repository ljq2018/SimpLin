#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


// [[Rcpp::export]]
List SimpLinCpp(arma::vec x, arma::vec y) {
  int n = x.n_elem;
  arma::vec z(n, arma::fill::ones);
  arma::mat X = arma::join_rows(z,x);
  arma::mat Xt = arma::trans(X);
  arma::mat beta = arma::inv(Xt * X) * Xt * y;
  arma::mat y_hat = X * beta;
  arma::mat res = y - y_hat;
  double MSE = (arma::trans(res) * res / (n-2))[0];
  arma::mat SE_mat = sqrt(arma::inv(Xt * X) * MSE);
  arma::mat SE = SE_mat.diag();
  double t = R::qt(0.975, n -2, 1, 0);
  arma::mat beta_UB = beta + t * SE;
  arma::mat beta_LB = beta - t * SE;
  arma::mat beta_CI = arma::join_rows(beta_LB,beta_UB);

  List model = List::create( Named("coefficients") = beta,
                             Named("SE") = SE,
                             Named("CI") = beta_CI,
                             Named("residuals") = res,
                             Named("predicted") = y_hat);

  return model;
}
