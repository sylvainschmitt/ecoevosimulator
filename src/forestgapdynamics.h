#ifndef FORESTGAPDYNAMICS_H
#define FORESTGAPDYNAMICS_H

#include <Rcpp.h>
Rcpp::NumericMatrix forestgapdynamics(
    int grid,
    double Pfall,
    int Rgaps
);
#endif
