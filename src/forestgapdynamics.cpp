#include <Rcpp.h>
using namespace Rcpp;

//' @title Forest gap dynamics
//' 
//' @description
//' C++ code for forest gap dynamics
//' 
//' @name forestgapdynamics
//' 
//' @param grid int.Number of cell per side of the matrix
//' @param Pfall double. Treefall probability
//' @param Rgaps int. Treefall gaps radius
//' 
//' @examples
//' forestgapdynamics(20, 2, 0.01)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix forestgapdynamics(
    int grid,
    double Pfall,
    int Rgaps
){
  NumericMatrix G(grid, grid) ;
  int xmin, xmax, ymin, ymax, r ;
  for (int x = 0; x < grid; x++){ // rows
    for (int y = 0; y < grid; y++){ // cols
      if(runif(1)[0] <= Pfall){ // treefall
        xmin = 0 ; // limits
        xmax = grid ;
        ymin = 0 ;
        ymax = grid ;
        if(x - Rgaps > xmin)
          xmin = x - Rgaps ;
        if(x + Rgaps < xmax)
          xmax = x + Rgaps ;
        if(y - Rgaps > ymin)
          ymin = y - Rgaps ;
        if(y + Rgaps < ymax)
          ymax = y + Rgaps ;
        for (int x2 = xmin; x2 < xmax; x2++){ // neighbours rows
          for (int y2 = ymin; y2 < ymax; y2++){ // neighbours cols
            r = sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2)) ;
            if(r < Rgaps){
              G(x2,y2) = 1 ; // killing the tree
            }
          }
        }
      }
    }
  }
  return G ;
}
