#include <Rcpp.h>
#include "disperse.h"  
using namespace Rcpp;

//' @title simulator C++
//' 
//' @description
//' Eco-evolutionary simulator in C++.
//' 
//' @name simulatorCpp
//' 
//' @param Topo matrix. Topography matrix generated with `sinusoidalTopogrpahy`, 
//'                     `squareDiamondTopography`, `paracouTopography`.
//' @param NCI matrix. NCI matrix generated with `generateNCI`.
//' @param grid int. Number of cell per side of the matrix.
//' @param Nt int. Number of time steps.
//' @param timestep int. Time-step length in years.
//' @param sigmaGtopo double. Variance of genetic values with topography.
//' @param sigmaZtopo double. Plasticity of phenotypes with topography.
//' @param sigmaGnci double. Variance of genetic values with NCI.
//' @param sigmaZnci double. Plasticity of phenotypes with NCI.
//' @param Pdeath double. Background mortality probability.
//' @param Ns int. Number of seedlings per cell.
//' @param Rpollination int. Pollination radius in cells (father to mother).
//' @param Rdispersion int. Dispersal radius in cells (mother to seedling).
//' @param determinist bool. Deterministic or probabilistic viability.
//' 
//' @return List of 6 Topography, Atopo, Ztopo, NCI, Anci, Znci.
//' 
//' @examples
//' simulatorCpp(Topo = sinusoidalTopography(grid = 10, Elim = 5, amplitude = 0.01), 
//'              NCI = generateNCIsim(grid = 10, Nt = 50))
//' 
//' @export
// [[Rcpp::export]]
List simulatorCpp(
    Rcpp::NumericMatrix Topo,
    Rcpp::NumericMatrix NCI,
    int grid = 10,
    int Nt = 50,
    int timestep = 30,
    double sigmaGtopo = 1,
    double sigmaZtopo = 1,
    double sigmaGnci = 2.651,
    double sigmaZnci = 2.651,
    double Pdeath = 0.01325548,
    int Ns = 4,
    int Rpollination = 1,
    int Rdispersion = 1,
    bool determinist = true
) {
  int Nind = grid*grid ;
  
  // Inputs check
  if (Topo.rows() != grid) {
    stop("Topography must be of size grid x grid.");
  }
  if (Topo.cols() != grid) {
    stop("Topography must be of size grid x grid.");
  }
  if (NCI.rows() != Nt) {
    stop("NCI must be of size Nt x grid^2.");
  }
  if (NCI.cols() != Nind) {
    stop("NCI must be of size Nt x grid^2.");
  }
  
  // Matrices for values at one generation and the next
  NumericMatrix Atopogen(grid, grid) ;
  NumericMatrix Ztopogen(grid, grid) ;
  NumericMatrix Atoponext(grid, grid) ;
  NumericMatrix Ztoponext(grid, grid) ;
  NumericMatrix NCIgen(grid, grid) ;
  NumericMatrix Ancigen(grid, grid) ;
  NumericMatrix Zncigen(grid, grid) ;
  NumericMatrix Ancinext(grid, grid) ;
  NumericMatrix Zncinext(grid, grid) ;
  
  // Matrices stocking all values
  NumericMatrix Topography(Nt, Nind) ; // topography
  NumericMatrix Atopo(Nt, Nind) ; // genotypes
  NumericMatrix Ztopo(Nt, Nind) ; // phenotypes
  NumericMatrix Anci(Nt, Nind) ; // genotypes
  NumericMatrix Znci(Nt, Nind) ; // phenotypes
 
  // cycles
  bool dead ;
  IntegerVector individual(2),  mother(2), father(2), seeds = seq(0, Ns-1) ;
  NumericVector atopooffsprings(Ns), ztopooffsprings(Ns) ;
  NumericVector ancioffsprings(Ns), zncioffsprings(Ns) ;
  NumericVector viability(Ns) ;
  int winner ;

  // Initialisation
  double muTopo = mean(Topo), muNCI = mean(NCI) ;
  double sigmaTopo = sd(Topo), sigmaNCI = sd(NCI) ;
  Topography.row(0) = Topo ;
  for(int i = 0; i < Nind; i++){
    Atopogen[i] = rnorm(1, muTopo, sigmaGtopo)[0] ;
    Ztopogen[i] = rnorm(1, Atopogen[i], sigmaZtopo)[0] ;
    Ancigen[i] = rnorm(1, muNCI, sigmaGnci)[0] ;
    Zncigen[i] = rnorm(1, Ancigen[i], sigmaZnci)[0] ;
  }
  Atopo.row(0) = Atopogen ;
  Ztopo.row(0) = Ztopogen  ;
  Anci.row(0) = Ancigen ;
  Znci.row(0) = Zncigen  ;
  
  // cycles
  for (int t = 1; t < Nt; t++){ // gens
    
    // NCI
    NumericVector temp = NCI.row(t-1) ;
    temp.attr("dim") = Dimension(grid, grid) ;
    NCIgen = as<NumericMatrix>(temp) ;
    
    for (int x = 0; x < grid; x++){ // rows
      for (int y = 0; y < grid; y++){ // cols

        // mortality
        dead = false ;
        if(rbinom(1, timestep, Pdeath)[0] > 0) dead = true ; //  i.e. there has been at least one death event
          
        if(dead){
          
          // reproduction
          individual[0] = x ;
          individual[1] = y ;
          for (int s = 0; s < Ns; s++){
            mother = disperse(individual, Rdispersion, grid) ;
            father = disperse(mother, Rpollination, grid) ;
            atopooffsprings(s) = rnorm(1, (Atopogen(mother[0],mother[1]) + Atopogen(father[0],father[1]))/2, sigmaGtopo/2)[0] ;
            ztopooffsprings(s) = rnorm(1, atopooffsprings(s), sigmaZtopo)[0] ;
            ancioffsprings(s) = rnorm(1, (Ancigen(mother[0],mother[1]) + Ancigen(father[0],father[1]))/2, sigmaGnci/2)[0] ;
            zncioffsprings(s) = rnorm(1, ancioffsprings(s), sigmaZnci)[0] ;
          }
          
          // recruitment
          viability = 1/sqrt(( (ztopooffsprings - Topo(x,y))/sigmaTopo )*( (ztopooffsprings - Topo(x,y))/sigmaTopo ) +
            ( (zncioffsprings - NCIgen(x,y))/sigmaNCI )*( (zncioffsprings - NCIgen(x,y))/sigmaNCI )) ;
          if(determinist){
            winner = which_max(viability) ;
          } else {
            winner = sample(seeds, 1, true, viability)[0] ;
          }
          Atoponext(x,y) = atopooffsprings(winner) ;
          Ztoponext(x,y) = ztopooffsprings(winner) ;
          Ancinext(x,y) = ancioffsprings(winner) ;
          Zncinext(x,y) = zncioffsprings(winner) ;
          
        } else {
          Atoponext(x,y) = Atopogen(x,y) ;
          Ztoponext(x,y) = Ztopogen(x,y) ;
          Ancinext(x,y) = Ancigen(x,y) ;
          Zncinext(x,y) = Zncigen(x,y) ;
        }
        
      }
    }
  
    // saving values from the generation
    Atopogen = Atoponext ;
    Ztopogen = Ztoponext ;
    Ancigen = Ancinext ;
    Zncigen = Zncinext ;
    Topography.row(t) = Topo ;
    Atopo.row(t) = Atopogen ;
    Ztopo.row(t) = Ztopogen ;
    Anci.row(t) = Ancigen ;
    Znci.row(t) = Zncigen ;
  }
  
  // return
  List sim = List::create(
    Named("Topography") = Topography,
    Named("Atopo") = Atopo, 
    Named("Ztopo") = Ztopo,
    Named("NCI") = NCI,
    Named("Anci") = Anci, 
    Named("Znci") = Znci
    ) ;
  return sim;
}
