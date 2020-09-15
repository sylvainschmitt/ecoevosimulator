#ifndef BUILD_GRADIENT_H
#define BUILD_GRADIENT_H

#include <Rcpp.h>
Rcpp::NumericVector build_gradient(
    double gradientlim,
    int length
);
#endif
