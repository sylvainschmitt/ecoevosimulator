#' @include utils-pipe.R rspcor.R rdeltanci.R
#' @importFrom dplyr mutate rename filter select left_join bind_rows
#' @importFrom reshape2 dcast melt
#' @importFrom raster as.matrix aggregate disaggregate raster values
NULL

#' Generate NCI over space and time
#'
#' @param grid int. Number of cells per side of the matrix.
#' @param Nt int. Number of time-steps.
#' @param timestep int. Time-step length in years.
#' @param muNCI double. mu parameter for the normal distribution used for NCI.
#' @param sigmaNCI double. sigma parameter for the normal distribution used for
#'   NCI.
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
#' @param d int. Spatial auto-correlation size in number of cells.
#'
#' @return A data frame with columns Time, X, Y and NCI.
#' 
#' @export
#'
#' @examples
#' generateNCI()
generateNCI <- function(
  grid = 20, # grid size
  Nt = 50, # number of time-steps
  timestep = 30, # time-step length in years
  muNCI = 124, # mu of normal distribution for NCI
  sigmaNCI = 26, # sigma of normal distribution NCI
  alpha = -1.32,
  beta = 0.003,
  mu = 0.749, # mu of lognormal distribution for positive deltaNCI
  sigma = 2.651, # sigma of lognormal distribution for positive deltaNCI
  lambda = 0.31, # lambda of exponential distribution for negative deltaNCI
  d = 3 # spatial auto-correlation size (3*3m)
){
  Var1 <- Var2 <- value <- Year <- NULL
  
  nci_y <- rnorm(grid*grid, 
                 mean = muNCI, 
                 sd = sigmaNCI)
  nci_y <- matrix(nci_y, nrow = grid, ncol = grid)
  nci_y <- raster(nci_y)
  nci_y <- aggregate(nci_y, d)
  
  nci <- as.matrix(disaggregate(nci_y, d, method = "bilinear"))[1:grid,1:grid] %>%
    melt() %>%
    rename(X = Var1, Y = Var2) %>% 
    mutate(Year = 1) %>%
    rename(NCI = value)
  
  for(y in 2:(timestep*(Nt-1))){
    raster::values(nci_y) <- raster::values(nci_y) + rdeltanci(raster::values(nci_y), 
                                                               alpha = alpha, 
                                                               beta = beta,
                                                               mu = mu, 
                                                               sigma = sigma, 
                                                               lambda = lambda)
    if(y %in% (timestep*(1:Nt))){
      nci <- bind_rows(nci,  
                       as.matrix(disaggregate(nci_y, d, method = "bilinear"))[1:grid,1:grid] %>%
                         melt() %>%
                         rename(X = Var1, Y = Var2) %>% 
                         mutate(Year = y) %>%
                         rename(NCI = value)) 
    }
  }
    
  return(nci %>% 
           group_by(Year) %>% 
           mutate(Ind = 1:n()) %>% 
           ungroup %>% 
           dcast(Year ~ Ind, value.var = "NCI") %>% 
           select(-Year) %>% 
           as.matrix())
}
