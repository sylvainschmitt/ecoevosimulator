#include <Rcpp.h>
#include "product_environmental_matrix.h"
#include "forestgapdynamics.h"
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
//' @param gapradius int. Gaps radius
//' @param fallprobability double. Gaps probability
//' @param death double. Death probabilty
//' @param viability_deterministic bool. Deterministic or probabilistic vaibility
//' 
//' @return A lsit.
//' 
//' @examples
//' simulatorCpp()
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
    int gapradius = 2,
    double fallprobability = 0.01,
    double death = 0.1 ,
    bool viability_deterministic = true
) {
  int Nind = grid*grid ;
  
  // Matrices stocking all values
  NumericMatrix E(Ngen, Nind) ; // ecotypes
  NumericMatrix A(Ngen, Nind) ; // genotypes
  NumericMatrix Z(Ngen, Nind) ; // phenotypes
  
  // Matrices for values at one generation
  NumericMatrix Egen = product_environmental_matrix(Elim, grid) ;
  NumericMatrix Agen(grid, grid) ;
  NumericMatrix Zgen(grid, grid) ;
  
  // objects for seedlings
  bool dead ;
  IntegerVector individual(2),  mother(2), father(2), seeds = seq(0, seedlings-1) ;
  NumericVector aoffsprings(seedlings), zoffsprings(seedlings), viability(seedlings) ;
  int winner ;
  NumericMatrix Anext(grid, grid) ;
  NumericMatrix Znext(grid, grid) ;
  NumericMatrix treefalls(grid, grid) ;

  // Gradient init with random draw
  E.row(0) = Egen ;
  for(int i = 0; i < Nind; i++){
    Agen[i] = rnorm(1, muG, sigmaG)[0] ;
    Zgen[i] = Agen[i] + rnorm(1, muE, sigmaE)[0] ;
  }
  A.row(0) = Agen ;
  Z.row(0) = Zgen  ;
  
  // simulation
  for (int g = 1; g < Ngen; g++){ // gens
    
    // treefalls
    treefalls = forestgapdynamics(grid, gapradius, fallprobability) ;
    
    for (int x = 0; x < grid; x++){ // rows
      for (int y = 0; y < grid; y++){ // cols
        
        individual[0] = x ;
        individual[1] = y ;
        
        // death
        dead = false ;
        if(runif(1)[0] <= death) dead = true ;
        if(treefalls(x,y) == 1) dead = true ;
          
        if(dead){
          // dispersal
          for (int s = 0; s < seedlings; s++){
            mother = disperse(individual, dispersal, 0, grid-1, 0, grid-1) ;
            father = disperse(mother, dispersal, 0, grid-1, 0, grid-1) ;
            aoffsprings(s) = rnorm(1, (Agen(mother[0],mother[1]) + Agen(father[0],father[1]))/2, sigmaG/2)[0] ;
            zoffsprings(s) = aoffsprings(s) + rnorm(1, muE, sigmaE)[0] ;
          }
          // viability
          viability = 1/sqrt((zoffsprings - Egen(x,y))*(zoffsprings - Egen(x,y))) ;
          if(viability_deterministic){
            winner = which_max(viability) ;
          } else {
            winner = sample(seeds, 1, true, viability)[0] ;
          }
          Anext(x,y) = aoffsprings(winner) ;
          Znext(x,y) = zoffsprings(winner) ;
        } else {
          Anext(x,y) = Agen(x,y) ;
          Znext(x,y) = Zgen(x,y) ;
        }
        
      }
    }
  
    // saving values from the generation
    Agen = Anext ;
    Zgen = Znext ;
    E.row(g) = Egen ;
    A.row(g) = Agen ;
    Z.row(g) = Zgen ;
  }
  List sim = List::create(Named("A") = A, 
                          Named("Z") = Z,
                          Named("E") = E) ;
  return sim;
}
