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
//' @param radius int. Gaps radius
//' @param probability double. Gaps probability
//' 
//' @examples
//' forestgapdynamics(20, 2, 0.01)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix forestgapdynamics(
    int grid,
    int radius,
    double probability
){
  NumericMatrix G(grid, grid) ;
  int xmin, xmax, ymin, ymax, r ;
  for (int x = 0; x < grid; x++){ // rows
    for (int y = 0; y < grid; y++){ // cols
      if(runif(1)[0] <= probability){ // treefall
        xmin = 0 ; // limits
        xmax = grid - 1 ;
        ymin = 0 ;
        ymax = grid - 1 ;
        if(x - radius > xmin)
          xmin = x - radius ;
        if(x + radius < xmax)
          xmax = x + radius ;
        if(y - radius > ymin)
          ymin = y - radius ;
        if(y + radius < ymax)
          ymax = y + radius ;
        for (int x2 = xmin; x2 < xmax; x2++){ // neighbours rows
          for (int y2 = ymin; y2 < ymax; y2++){ // neighbours cols
            r = sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2)) ;
            if(r < radius){
              G(x2,y2) = 1 ; // killing the tree
            }
          }
        }
      }
    }
  }
  return G ;
}
