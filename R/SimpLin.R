# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

SimpLin1 <- function(X,Y) {
  n = length(X)
  X_mat <- matrix(data = c(rep(1,n),X), ncol = 2, nrow = n)
  Y_mat <- matrix(data = Y, ncol = 1, nrow = n)

  betas <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% Y_mat
  value <- c("beta_0", "beta_1")

  y_hat <-X_mat%*%betas
  mse <- sum((Y_mat-y_hat)^2)/n
  var <- diag(mse*(solve(t(X_mat)%*%X_mat)))
  stderr <- t(t(sqrt(var)))


  lower_ci <- betas[1,] + qt(0.025,n-2)*stderr
  upper_ci <- betas[1,] + qt(0.975,n-2)*stderr
  ci <- matrix(data = c(lower_ci, upper_ci), nrow = 2, ncol = 2)
;
  return(data.frame(value,betas,stderr,lower_ci,upper_ci))
}

X <- c(1,2,3)
Y <- c(8,5,6)
SimpLin1(X,Y)

library("Rcpp")
sourceCpp("SimpLin.cpp")

