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
//' @param grid int. Number of cell per side of the matrix
//' @param Nt int. Number of time steps
//' @param Elim double. Environmental gradient size
//' @param muG double. Mean of genetic values
//' @param sigmaG double. Variance of genetic values
//' @param muE double. Mean of environmental values
//' @param sigmaE double. Variance of environmental values
//' @param Pfall double. Treefall probability
//' @param Rgaps int. Treefall gaps radius
//' @param Pdeath double. Background mortality probability
//' @param Ns int. Number of seedlings per cell
//' @param Rdispersal int. Dispersal radius in cells
//' @param determinist bool. Deterministic or probabilistic vaibility
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
    int Nt = 50,
    double Elim = 5,
    double muG = 0,
    double sigmaG = 1,
    double muE = 0,
    double sigmaE = 1,
    double Pfall = 0.01,
    int Rgaps = 2,
    double Pdeath = 0.1 ,
    int Ns = 4,
    int Rdispersal = 1,
    bool determinist = true
) {
  int Nind = grid*grid ;
  
  // Matrices stocking all values
  NumericMatrix E(Nt, Nind) ; // ecotypes
  NumericMatrix A(Nt, Nind) ; // genotypes
  NumericMatrix Z(Nt, Nind) ; // phenotypes
  
  // Matrices for values at one generation and the next
  NumericMatrix Egen = product_environmental_matrix(Elim, grid) ;
  NumericMatrix Agen(grid, grid) ;
  NumericMatrix Zgen(grid, grid) ;
  NumericMatrix Anext(grid, grid) ;
  NumericMatrix Znext(grid, grid) ;
  
  // mortality
  NumericMatrix treefalls(grid, grid) ;
  bool dead ;
  
  // reproduction & recruitment
  IntegerVector individual(2),  mother(2), father(2), seeds = seq(0, Ns-1) ;
  NumericVector aoffsprings(Ns), zoffsprings(Ns), viability(Ns) ;
  int winner ;

  // Initialisation
  E.row(0) = Egen ;
  for(int i = 0; i < Nind; i++){
    Agen[i] = rnorm(1, muG, sigmaG)[0] ;
    Zgen[i] = Agen[i] + rnorm(1, muE, sigmaE)[0] ;
  }
  A.row(0) = Agen ;
  Z.row(0) = Zgen  ;
  
  // cycles
  for (int t = 1; t < Nt; t++){ // gens
    
    // treefalls
    treefalls = forestgapdynamics(grid, Pfall, Rgaps) ;
    
    for (int x = 0; x < grid; x++){ // rows
      for (int y = 0; y < grid; y++){ // cols

        // mortality
        dead = false ;
        if(treefalls(x,y) == 1) dead = true ;
        if(runif(1)[0] <= Pdeath) dead = true ;
          
        if(dead){
          
          // reproduction
          individual[0] = x ;
          individual[1] = y ;
          for (int s = 0; s < Ns; s++){
            mother = disperse(individual, Rdispersal, grid) ;
            father = disperse(mother, Rdispersal, grid) ;
            aoffsprings(s) = rnorm(1, (Agen(mother[0],mother[1]) + Agen(father[0],father[1]))/2, sigmaG/2)[0] ;
            zoffsprings(s) = aoffsprings(s) + rnorm(1, muE, sigmaE)[0] ;
          }
          
          // recruitment
          viability = 1/sqrt((zoffsprings - Egen(x,y))*(zoffsprings - Egen(x,y))) ;
          if(determinist){
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
    E.row(t) = Egen ;
    A.row(t) = Agen ;
    Z.row(t) = Zgen ;
  }
  
  // return
  List sim = List::create(Named("A") = A, 
                          Named("Z") = Z,
                          Named("E") = E) ;
  return sim;
}
