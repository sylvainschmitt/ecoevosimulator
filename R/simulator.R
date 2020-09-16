#' @include utils-pipe.R
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom reshape2 melt
NULL

#' simulator
#' 
#' @param grid int.  Number of cell per side of the environmental matrix
#' @param Ngen int. Number of generations
#' @param muG double. Mean of genetic values
#' @param sigmaG double. Variance of genetic values
#' @param muE double. Mean of environmental values
#' @param sigmaE double. Variance of environmental values
#' @param Elim double. Environmental gradient size
#' @param seedlings int. Number of seedlings per cell
#' @param dispersal int. Dispersal distance in cells
#' @param viability_deterministic bool. Deterministic or probabilistic vaibility
#'
#' @return A data frame.
#' 
#' @export
#'
#' @examples
#' 
#' simulator()
#' 
simulator <- function(
  grid = 20,
  Ngen = 50,
  muG = 0,
  sigmaG = 1,
  muE = 0,
  sigmaE = 1,
  Elim = 5,
  seedlings = 4,
  dispersal = 1,
  viability_deterministic = TRUE
){
  sim <- simulatorCpp(grid, Ngen, muG, sigmaG, muE, sigmaE, Elim, seedlings, dispersal, viability_deterministic)
  coords <- data.frame(
    individual = 1:(grid*grid),
    X = rep(1:grid, each = grid),
    Y = rep(1:grid, grid)
  )
  lapply(list("ecotype" = sim$E, 
              "genotype" = sim$A, 
              "phenotype" = sim$Z), 
         function(M)
    melt(M) %>% 
    rename(generation = Var1, individual = Var2)) %>% 
    bind_rows(.id = "var") %>% 
    left_join(coords, by = "individual")
}
  