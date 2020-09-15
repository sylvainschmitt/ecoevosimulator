#include <Rcpp.h>
#include "build_gradient.h"
using namespace Rcpp;

//' @title simulator C++
//' 
//' @description
//' Eco-evolutionary simulator in C++.
//' 
//' @name simulatorCpp
//' 
//' @param Nind int. Number of individuals
//' @param Ngen int. Number of generations
//' @param muG double. Mean of genetic values
//' @param sigmaG double. Variance of genetic values
//' @param muE double. Mean of environmental values
//' @param sigmaE double. Variance of environmental values
//' @param Elim double. Environmental gradient size
//' @param seedlings int. Number of seedlings per cell
//' @param dispersal int. Dispersal distance in cells
//' @param viability_deterministic bool. Deterministic or probabilistic vaibility
//' 
//' @return A lsit.
//' 
//' @examples
//' simulator()
//' 
//' @export
// [[Rcpp::export]]
List simulatorCpp(
    int  Nind = 50,
    int Ngen = 50,
    double muG = 0,
    double sigmaG = 1,
    double muE = 0,
    double sigmaE = 1,
    double Elim = 10,
    int seedlings = 4,
    int dispersal = 1,
    bool viability_deterministic = true
) {
  NumericMatrix A(Ngen, Nind) ;
  NumericMatrix Z(Ngen, Nind) ;
  NumericVector E = build_gradient(Elim, Nind) ;
  NumericMatrix Aoffsprings(Nind, seedlings) ;
  NumericMatrix Zoffsprings(Nind, seedlings) ;
  NumericVector w(seedlings) ;
  IntegerVector seeds(seedlings) ;
  int imin , imax, winner ;
  double muS ;
  A.row(0) = rnorm(Nind, muG, sigmaG) ; 
  Z.row(0) = rnorm(Nind, muE, sigmaE) ;
  for(int s = 0; s < seedlings; s++)
    seeds(s) = s ;
  for (int g = 1; g < Ngen; g++){
    for (int i = 0; i < Nind; i++){
      imin = 0 ;
      imax = Nind ;
      if(i-dispersal > 0){
        imin = i-dispersal ;
      } 
      if(i+dispersal+1 < Nind){
        imax = i+dispersal+1 ;
      }
      NumericVector Am(imax-imin) ;
      IntegerVector Pm(imax-imin) ;
      for(int m = 0; m < imax-imin; m++){
        Pm(m) = m ;
        Am(m) = A(g-1,imin+m) ;
      } 
      for (int s = 0; s < seedlings; s++){
        int M = sample(Pm, 1)[0] ;
        NumericVector Af(imax-imin) ; // imax and imin to be redefined
        for(int m = 0; m < imax-imin; m++){
          Af(m) = A(g-1,imin+m) ;
        } 
        Aoffsprings(i,s) = rnorm(1, (Am[M] + sample(Af, 1)[0])/2, sigmaG/2)[0] ;
        Zoffsprings(i,s) = Aoffsprings(i,s) + rnorm(1, muE, sigmaE)[0] ;
      }
      if(viability_deterministic){
        winner = which_min(sqrt(pow(Zoffsprings(i,_)-E(i), 2))) ; 
      } else {
        w = 1/sqrt(pow(Zoffsprings(i,_)-E(i), 2)) ;
        winner = sample(seeds, 1, true, w)[0] ;
      }
      A(g,i) = Aoffsprings(i,winner) ;
      Z(g,i) =  Zoffsprings(i,winner) ;
    }
  }
  List sim = List::create(Named("A") = A, 
                          Named("Z") = Z,
                          Named("E") = E) ;
  return sim;
}
