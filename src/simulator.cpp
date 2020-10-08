#include <Rcpp.h>
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
//' @param Topography matrix. Environmental matrix
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
//' simulatorCpp(Topography = sinusoidalTopography(grid = 10, Elim = 5, amplitude = 0.01))
//' 
//' @export
// [[Rcpp::export]]
List simulatorCpp(
    Rcpp::NumericMatrix Topography,
    int grid = 10,
    int Nt = 50,
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
  if (Topography.rows() != grid) {
    stop("Topography must be of size grid.");
  }
  
  // Matrices stocking all values
  NumericMatrix Etopo(Nt, Nind) ; // topography
  NumericMatrix Egaps(Nt, Nind) ; // forest gap dynamics
  NumericMatrix A(Nt, Nind) ; // genotypes
  NumericMatrix Z(Nt, Nind) ; // phenotypes
  
  // Matrices for values at one generation and the next
  NumericMatrix Agen(grid, grid) ;
  NumericMatrix Zgen(grid, grid) ;
  NumericMatrix Anext(grid, grid) ;
  NumericMatrix Znext(grid, grid) ;
  NumericMatrix Gaps(grid, grid) ;
 
  // cycles
  bool dead ;
  IntegerVector individual(2),  mother(2), father(2), seeds = seq(0, Ns-1) ;
  NumericVector aoffsprings(Ns), zoffsprings(Ns), viability(Ns) ;
  int winner ;

  // Initialisation
  Etopo.row(0) = Topography ;
  for(int i = 0; i < Nind; i++){
    Agen[i] = rnorm(1, muG, sigmaG)[0] ;
    Zgen[i] = Agen[i] + rnorm(1, muE, sigmaE)[0] ;
  }
  A.row(0) = Agen ;
  Z.row(0) = Zgen  ;
  
  // cycles
  for (int t = 1; t < Nt; t++){ // gens
    
    // treefalls
    Gaps = forestgapdynamics(grid, Pfall, Rgaps) ;
    
    for (int x = 0; x < grid; x++){ // rows
      for (int y = 0; y < grid; y++){ // cols

        // mortality
        dead = false ;
        if(Gaps(x,y) == 1) dead = true ;
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
          viability = 1/sqrt((zoffsprings - Topography(x,y))*(zoffsprings - Topography(x,y))) ;
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
    Etopo.row(t) = Topography ;
    Egaps.row(t) = Gaps ;
    A.row(t) = Agen ;
    Z.row(t) = Zgen ;
  }
  
  // return
  List sim = List::create(Named("A") = A, 
                          Named("Z") = Z,
                          Named("Etopo") = Etopo,
                          Named("Egaps") = Egaps) ;
  return sim;
}
