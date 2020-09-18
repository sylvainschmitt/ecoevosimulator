#include <Rcpp.h>
using namespace Rcpp;

//' @title Disperse
//' 
//' @description
//' C++ code for dispersal
//' 
//' @name disperse
//' 
//' @param a IntegerVector. Final dispesal positions
//' @param Rdispersal int. Dispersal radius in cells
//' @param grid int. Number of cell per side of the matrix
//' 
//' @examples
//' disperse(c(10, 10), 2, 20)
//' 
//' @export
// [[Rcpp::export]]
IntegerVector disperse(
    IntegerVector a,
    int Rdispersal,
    int grid
){
  int xbmin = 0 ;
  int xbmax = grid-1 ;
  int ybmin = 0 ;
  int ybmax = grid-1 ;
  IntegerVector b(2) ;
  double r ;
  if(a[0]-Rdispersal > 0)
    xbmin = a[0]-Rdispersal ;
  if(a[0]+Rdispersal < grid-1)
    xbmax = a[0]+Rdispersal ;
  if(a[1]-Rdispersal > 0)
    ybmin = a[1]-Rdispersal ;
  if(a[1]+Rdispersal < grid-1)
    ybmax = a[1]+Rdispersal ;
  IntegerVector posxb = seq(xbmin, xbmax) ;
  IntegerVector posyb = seq(ybmin, ybmax) ;
  for(int i = 0; i < pow(10,Rdispersal); i++){
    b[0] = sample(posxb, 1)[0] ; // beware uniform sampling
    b[1] = sample(posyb, 1)[0] ; // beware uniform sampling
    r = sqrt((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])) ;
    if(r <= Rdispersal) break ;
  }
  return b ;
}
