
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo]]

using namespace arma;

//[[Rcpp::export()]]

arma:: colvec SimpLin(arma:: vec x, arma:: vec y){

  int n = x.size();
  int l = y.size();

  if (n != l)Rcpp:: stop("X and Y must have the same length");

  ones = x.ones();
  x_mat = join_vert(ones, x);

  arma :: mat betas = solve(x_mat.t(), x_mat)%x_mat.t()%y;

  arma:: mat y_hat = x_mat%betas;

  arma:: vec diff_sq = pow(y_mat-y_hat,2);

  int mse = diff_sq.sum()/n;
  arma:: mat var = diag(mse*solve(x_mat.t(),x_mat));
  arma:: mat stderr = pow(trans(x_mat.t()), 1/2);

  arma:: mat lower_ci = betas[1, ] - R::qt(0.975,0,1,1,0)*stderr[1, ];
  arma:: mat upper_ci = betas[1, ] + R::qt(0.975,0,1,1,0)*stderr[1, ];

  CharacterVector v = {"beta0", "beta1"};
  values = CharacterMatrix m(2,1, v.begin() );

  DataFrame df = DataFrame::create(Named("coeff") = values,
                                   Named("values") = betas,,
                                   Named("stderr") = stderr,
                                   Named("lower_ci") = lower_ci,
                                   Named("upper_ci") = upper_ci);

  return df;

}



