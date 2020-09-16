#include <Rcpp.h>
#include "product_environmental_matrix.h"
#include "disperse.h"  
using namespace Rcpp;

//' @title simulator C++
//' 
//' @description
//' Eco-evolutionary simulator in C++.
//' 
//' @name simulatorCpp
//' 
//' @param grid int. Number of cell per side of the environmental matrix
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
    int grid = 20,
    int Ngen = 50,
    double muG = 0,
    double sigmaG = 1,
    double muE = 0,
    double sigmaE = 1,
    double Elim = 5,
    int seedlings = 4,
    int dispersal = 1,
    bool viability_deterministic = true
) {
  int Nind = grid*grid ;
  
  NumericMatrix E(Ngen, Nind) ; // environmental values
  NumericMatrix A(Ngen, Nind) ; // genetic values
  NumericMatrix Z(Ngen, Nind) ; // phenotypic values
  
  NumericMatrix Egen = product_environmental_matrix(Elim, grid) ; // Environmental matrix
  NumericMatrix Agen(grid, grid) ; // genetic values for one generation
  NumericMatrix Zgen(grid, grid) ; // phenotypic values for one generation
  
  // NumericMatrix Aoffsprings(Nind, seedlings) ; // Offspring genetic values for one generation
  // NumericMatrix Zoffsprings(Nind, seedlings) ; // Offspring phenotypic values for one generation
  // 
  // NumericVector w(seedlings) ; // Seedlings probabilities for one cell
  // IntegerVector seeds = seq(0, seedlings) ; // seedlings for one cell
  // int mother, father, winner ; // mother, father and winner positions
  // double muS ;
  E.row(0) = Egen ;
  A.row(0) = rnorm(Nind, muG, sigmaG) ;   // Gradient init with random draw
  Z.row(0) = rnorm(Nind, muE, sigmaE) ;
  for(int i = 0; i < Nind; i++){
    Agen[i] = A.row(0)[i] ;
    Zgen[i] = Z.row(0)[i] ;
  }
  for (int g = 1; g < Ngen; g++){ // gens
  //   for (int i = 0; i < Nind; i++){ // inds
  //     for (int s = 0; s < seedlings; s++){
  //       mother = disperse(i, dispersal, 0, Nind) ;
  //       father = disperse(mother, dispersal, 0, Nind) ;
  //       Aoffsprings(i,s) = rnorm(1, (A(g-1,mother) + A(g-1,father))/2, sigmaG/2)[0] ;
  //       Zoffsprings(i,s) = Aoffsprings(i,s) + rnorm(1, muE, sigmaE)[0] ;
  //     }
  //     if(viability_deterministic){
  //       winner = which_min(sqrt(pow(Zoffsprings(i,_)-E(i), 2))) ; 
  //     } else {
  //       w = 1/sqrt(pow(Zoffsprings(i,_)-E(i), 2)) ;
  //       winner = sample(seeds, 1, true, w)[0] ;
  //     }
  //     A(g,i) = Aoffsprings(i,winner) ;
  //     Z(g,i) =  Zoffsprings(i,winner) ;
  //   }
  
  E.row(g) = Egen ;
  A.row(g) = Agen ;
  Z.row(g) = Zgen ;
  }
  List sim = List::create(Named("A") = A, 
                          Named("Z") = Z,
                          Named("E") = E) ;
  return sim;
}
