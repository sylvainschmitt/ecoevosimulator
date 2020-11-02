#' @importFrom stats rbinom rlnorm rexp
NULL

#' The deltaNCI distribution
#'
#' Random generator of deltaNCI based on a proability p to be negative used in a
#' Bernoulli distribution. Negative deltaNCI follow an exponential distribution
#' of parameter lambda. Positive deltaNCI follow a lognormal distribution of
#' parameters mu and sigma. Default parameters correspond to values inferred in
#' the control and biodiversity plots of Paracou.
#'
#' @param NCI double. Vector of previous NCI (size of the vector determines
#'   number of generated deltaNCI).
#' @param alpha double. Intercept for the Bernoulli distribution determining the
#'   risk to have a negative deltaNCI.
#' @param beta double. Slope of previous NCI for the Bernoulli distribution
#'   determining the risk to have a negative deltaNCI.
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
  NCI = rnorm(1, mean = 122.73, sd = 25.806),
  alpha = -1.32,
  beta = 0.003,
  mu = 0.749,
  sigma = 2.651,
  lambda = 0.31
){
  samples <- rep(0, length(NCI))
  P <- sapply(invlogit(alpha + beta*NCI), function(p) rbinom(1,1,p)) == 1
  samples[!P] <- rlnorm(sum(!P), meanlog = log(mu), sdlog = log(sigma))
  samples[P] <- -rexp(sum(P), rate = lambda)
  return(samples)
}
