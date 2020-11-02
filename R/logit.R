#' Logistic function
#'
#' To transform probabilities into reals with log( p /(1-p)).
#'
#' @param p double. A vector of probability.
#'
#' @return A vector of reals.
#'
#' @export
#'
#' @examples
#' logit(pnorm(rnorm(1)))
#' 
logit <- function(p){
  log( p/(1-p) ) 
}

#' Inverse of logistic function
#'
#' To transform reals into probabilities with exp(x) / (1 + exp(x)).
#' 
#' @param x double. A vector of reals.
#'
#' @return A vector of probabilities.
#' 
#' @export
#'
#' @examples
#' invlogit(rnorm(1))
#' 
invlogit <- function(x){
  exp(x) / (1 + exp(x))
}