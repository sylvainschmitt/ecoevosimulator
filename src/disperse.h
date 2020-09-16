#ifndef DISPERSE_H
#define DISPERSE_H

#include <Rcpp.h>
Rcpp::IntegerVector disperse(
        Rcpp::IntegerVector a,
        int d,
        int xmin,
        int xmax,
        int ymin,
        int ymax
);
#endif
