#ifndef PRODUCT_ENVIRONMENTAL_MATRIX_H
#define PRODUCT_ENVIRONMENTAL_MATRIX_H

#include <Rcpp.h>
Rcpp::NumericMatrix product_environmental_matrix(
    double gradientlim,
    int length
);
#endif

