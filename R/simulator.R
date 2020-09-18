#' @include utils-pipe.R
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom reshape2 melt
NULL

#' simulator
#' 
#' @param grid int.  Number of cells per side of the matrix
#' @param Nt int. Number of time steps
#' @param Elim double. Environmental gradient size
#' @param muG double. Mean of genetic values
#' @param sigmaG double. Variance of genetic values
#' @param muE double. Mean of environmental values
#' @param sigmaE double. Variance of environmental values
#' @param Pfall double. Treefall probability
#' @param Rgaps int. Treefall gaps radius
#' @param Pdeath double. Background mortality probability
#' @param Ns int. Number of seedlings per cell
#' @param Rdispersal int. Dispersal radius in cells
#' @param determinist bool. Deterministic or probabilistic vaibility
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
  Nt = 50,
  Elim = 5,
  muG = 0,
  sigmaG = 1,
  muE = 0,
  sigmaE = 1,
  Pfall = 0.01,
  Rgaps = 2,
  Pdeath = 0.1,
  Ns = 4,
  Rdispersal = 1,
  determinist = TRUE
){
  Var1 <- Var2 <- NULL
  sim <- simulatorCpp(grid, Nt, Elim,
                      muG, sigmaG, muE, sigmaE, 
                      Pfall, Rgaps, Pdeath, 
                      Ns, Rdispersal,
                      determinist)
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
  