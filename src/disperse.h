#ifndef DISPERSE_H
#define DISPERSE_H

#include <Rcpp.h>
Rcpp::IntegerVector disperse(
        Rcpp::IntegerVector a,
        int Rdispersal,
        int grid
);
#endif
