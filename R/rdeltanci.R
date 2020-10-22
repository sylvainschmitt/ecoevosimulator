#' The deltaNCI distribution
#'
#' Random generator of deltaNCI based on a proability p to be negative used in a
#' Bernoulli distribution. Negative deltaNCI follow an exponential distribution
#' of parameter lambda. Positive deltaNCI follow a lognormal distribution of
#' parameters mu and sigma. Default parameters correspond to values inferred in
#' the control and biodiversity plots of Paracou.
#'
#' @param n int. Number of observations.
#' @param p double. Probability to be negative used in the Bernoulli
#'   distribution.
#' @param mu double. mu parameter for the lognormal distribution used for
#'   positive deltaNCI.
#' @param sigma double. sigma parameter for the lognormal distribution used for
#'   positive deltaNCI.
#' @param lambda double. lambda parameter for the exponential distribution used
#'   for negative deltaNCI.
#'
#' @return A vector of n generated delta nci.
#' @export
#'
#' @examples
#' rdeltanci(1)
#' 
rdeltanci <- function(
  n = 1, # number of samples
  p = 0.271, # probability to be positive
  mu = 0.749, # mu of lognormal distributon for positive deltanci
  sigma = 2.651, # sigma of lognormal distributon for positive deltanci
  lambda = 0.31 # lamnda of exponential distributon for negative deltanci
){
  samples <- rep(0, n)
  P <- rbinom(n, 1, 1 - p) == 1
  samples[P] <- rlnorm(sum(P), meanlog = log(mu), sdlog = log(sigma))
  samples[!P] <- -rexp(sum(!P), rate = lambda)
  return(samples)
}
