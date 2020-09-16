#include <Rcpp.h>
using namespace Rcpp;

//' @title Disperse
//' 
//' @description
//' C++ code for dispersal
//' 
//' @name disperse
//' 
//' @param a int. Final dispesal position
//' @param d int. Dispersal distance
//' @param xmin int. Minimum cell coordinate
//' @param xmax int. Maximum cell coordinate
//' 
//' @examples
//' disperse(10, 10)
//' 
//' @export
// [[Rcpp::export]]
int disperse(
    int a,
    int d,
    int xmin,
    int xmax
){
  int bmin = xmin ;
  int bmax = xmax ;
  if(a-d > xmin){
    bmin = a-d ;
  } 
  if(a+d+1 < xmax){
    bmax = a+d+1 ;
  } 
  IntegerVector posb = seq(bmin, bmax) ;
  int b = sample(posb, 1)[0] ; // beware uniform sampling
  return b ;
}
