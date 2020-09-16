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
NumericMatrix product_environmental_matrix(
    double gradientlim,
    int length
){
  double step = gradientlim*2/(length-1) ;
  NumericVector gradient(length) ;
  NumericMatrix E(length, length) ;
  gradient[0] = - gradientlim ;
  for(int i = 1; i < length; i++)
    gradient[i] = gradient[i-1] + step ;
  for(int i = 0; i < length; i++){
    for(int j = 0; j < length; j++){
      E(i,j) = gradient[i]*gradient[j] ;
    }
  }
  return E ;
}
