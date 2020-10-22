#' @include utils-pipe.R
#' @importFrom dplyr mutate rename filter select left_join bind_rows
NULL

#' Generate NCI over space and time
#'
#' @param grid int. Number of cells per side of the matrix.
#' @param Nt int. Number of time steps.
#' @param muNCI double. mu parameter for the normal distribution used for NCI.
#' @param sigmaNCI double. sigma parameter for the normal distribution used for
#'   NCI.
#' @param p double. Probability to be negative used in the Bernoulli
#'   distribution.
#' @param mu double. mu parameter for the lognormal distribution used for
#'   positive deltaNCI.
#' @param sigma double. sigma parameter for the lognormal distribution used for
#'   positive deltaNCI.
#' @param lambda double. lambda parameter for the exponential distribution used
#'   for negative deltaNCI.
#' @param d int. Spatial autoccorelation size in number of cells.
#'
#' @return A data frame with columns Time, X, Y and NCI.
#' 
#' @export
#'
#' @examples
#' generateNCI()
generateNCI <- function(
  grid = 50, # grid size
  Nt = 50, # number of timesteps
  muNCI = 0.749, # mu of normal distributon for nci
  sigmaNCI = 2.651, # sigma of normal distributon nci
  p = 0.271, # probability to be positive
  mu = 0.749, # mu of lognormal distributon for positive deltanci
  sigma = 2.651, # sigma of lognormal distributon for positive deltanci
  lambda = 0.31, # lamnda of exponential distributon for negative deltanci
  d = 3 # spatial autocorrelation size (3*3m)
){
  NCI <- rspcor(grid = grid, 
                generator = "rnorm", 
                args = c(mean = muNCI, sd = sigmaNCI), 
                dcor = d) %>% 
    mutate(Time = 1) %>% 
    mutate(deltaNCI = NA) %>% 
    rename(NCI = value)
  for(t in 2:Nt){
    NCIy <- filter(NCI, Time == (t - 1)) %>% 
      select(-Time, -deltaNCI) %>% 
      left_join(rspcor(grid = grid, 
                       generator = "rdeltanci",
                       args = list(p = p,
                                   mu = mu, 
                                   sigma = sigma, 
                                   lambda = lambda),
                       dcor = d) %>% 
                  mutate(Time = t) %>% 
                  rename(deltaNCI = value), 
                by = c("X", "Y")) %>% 
      mutate(NCI = NCI + deltaNCI)
    NCI <- bind_rows(NCI, NCIy)   
  }
  return(select(NCI, Time, X, Y, NCI, -deltaNCI))
}
