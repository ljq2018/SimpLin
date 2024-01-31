
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//[[Rcpp::export()]]
DataFrame SimpLin(arma:: vec x, arma:: vec y){

  int n = x.size();
  int l = y.size();

  if (n != l)Rcpp:: stop("X and Y must have the same length");

  arma:: mat ones = x.ones();
  arma:: mat x_mat = join_cols(ones, x);

  arma:: mat betas = inv(x_mat.t()*x_mat)*x_mat.t()*y;

  arma:: mat y_hat = x_mat*betas;

  arma:: vec diff_sq = square(y-y_hat);

  int mse = sum(diff_sq)/n;
  arma:: mat var_mat = mse*inv(x_mat.t()*x_mat);
  arma:: vec stderr = sqrt(var_mat.diag());

  arma:: mat lower_ci = betas - R::qt(0.975,n-2,1,0)*stderr;
  arma:: mat upper_ci = betas + R::qt(0.975,n-2,1,0)*stderr;

  CharacterVector v = {"beta0", "beta1"};

  DataFrame df = DataFrame::create(Named("coeff") = v,
                                   Named("values") = betas,
                                   Named("stderr") = stderr,
                                   Named("lower_ci") = lower_ci,
                                   Named("upper_ci") = upper_ci);

 return df;
}



