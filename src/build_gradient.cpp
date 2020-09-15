#include <Rcpp.h>
using namespace Rcpp;

//' @title Build gradient
//' 
//' @description
//' C++ code to build environmental gradients
//' 
//' @name build_gradient
//' 
//' @param gradientlim double. Environmental gradient size
//' @param length int. Number of cells
//' 
//' @examples
//' build_gradient(10, 10)
//' 
//' @export
// [[Rcpp::export]]
NumericVector build_gradient(
    double gradientlim,
    int length
){
  double step = gradientlim*2/(length-1) ;
  NumericVector gradient(length) ;
  gradient[0] = - gradientlim ;
  for (int i = 1; i < length; i++)
    gradient[i] = gradient[i-1] + step ;
  return gradient ;
}
