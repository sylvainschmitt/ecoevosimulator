#include <Rcpp.h>
using namespace Rcpp;

//' @title Disperse
//' 
//' @description
//' C++ code for dispersal
//' 
//' @name disperse
//' 
//' @param xa IntegerVector. Final dispesal positions
//' @param d int. Dispersal distance
//' @param xmin int. Minimum row
//' @param xmax int. Maximum row
//' @param ymin int. Minimum column
//' @param ymax int. Maximum column
//' 
//' @examples
//' disperse(c(10, 10), 2, 0, 20, 0, 20)
//' 
//' @export
// [[Rcpp::export]]
IntegerVector disperse(
    IntegerVector a,
    int d,
    int xmin,
    int xmax,
    int ymin,
    int ymax
){
  int xbmin = xmin ;
  int xbmax = xmax ;
  int ybmin = ymin ;
  int ybmax = ymax ;
  IntegerVector b(2) ;
  double r ;
  if(a[0]-d > xmin)
    xbmin = a[0]-d ;
  if(a[0]+d+1 < xmax)
    xbmax = a[0]+d+1 ;
  if(a[1]-d > ymin)
    ybmin = a[1]-d ;
  if(a[1]+d+1 < ymax)
    ybmax = a[1]+d+1 ;
  IntegerVector posxb = seq(xbmin, xbmax) ;
  IntegerVector posyb = seq(ybmin, ybmax) ;
  for(int i = 0; i < pow(10,d); i++){
    b[0] = sample(posxb, 1)[0] ; // beware uniform sampling
    b[1] = sample(posyb, 1)[0] ; // beware uniform sampling
    r = sqrt((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])) ;
    // Rcout << "a = (" << a[0] << "," << a[1] << "); b = (" << b[0] << "," << b[1] << "); r = " << r << "\n" ;
    if(r > 0 && r <= d) break ;
  }
  return b ;
}
