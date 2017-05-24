#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// original R function
// function (y_pred, y_true) 
// {
//   eps <- 1e-15
//   y_pred <- pmax(y_pred, eps)
//   Poisson_LogLoss <- mean(log(gamma(y_true + 1)) + y_pred - 
//     log(y_pred) * y_true)
//   return(Poisson_LogLoss)
// }


// [[Rcpp::export]]
double poissonLogLoss(NumericVector predicted, NumericVector actual) {
  NumericVector temp, y_pred_new;
  double out; 
  const double eps=1e-15;
  
  y_pred_new=pmax(predicted,eps);
  temp = log(gamma(actual + 1)) + y_pred_new - log(y_pred_new)*actual;
  out=mean(temp); // using sugar implementation
  return out;
}