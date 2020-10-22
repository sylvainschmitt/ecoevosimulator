#' @include utils-pipe.R
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom reshape2 melt
NULL

#' simulator
#' 
#' @param grid int.  Number of cells per side of the matrix.
#' @param Nt int. Number of time steps.
#' @param topography char. Topography generator between 'sinusoidal', 'squarediamond' or 'paracou'
#' @param Elim double. Environmental matrix extrme (absolute value)
#' @param amplitude double. Amplitude of the sinusoidal functional
#' @param rudgeness double. Rugedness parameter
#' @param plot int. Plot number between 1 and 15 (a cell size is 3x3m)
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
  topography = "sinusoidal",
  Elim = 5,
  amplitude = 1,
  rudgeness = 1,
  plot = 1,
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
  if(!(topography %in% c('sinusoidal', 'squarediamond', 'paracou')))
    stop(paste(topography, "option doesn't exist to generate topography (see help)."))
  if(topography == 'sinusoidal')
    Topography <- sinusoidalTopography(grid = grid, 
                                       Elim = Elim, 
                                       amplitude = amplitude)
  if(topography == 'squarediamond'){
    Topography <- squareDiamondTopography(grid = 2^ceiling(log2(grid-1))+1, 
                                          rudgeness = rudgeness)
    Topography <- Topography[1:grid, 1:grid]
  }
  if(topography == 'paracou'){
    Topography <- paracouTopography(plot = plot,
                                    Elim = Elim)
    if(grid > ncol(Topography))
      stop(paste0("Using topography fom Paracou plot ", plot, ", the grid can't excess ", ncol(Topography)))
    Topography <- Topography[1:grid, 1:grid]
  }

  Var1 <- Var2 <- NULL
  sim <- simulatorCpp(
    Topography = Topography,
    grid = grid, 
    Nt = Nt, 
    muG = muG, 
    sigmaG = sigmaG,
    muE = muE,
    sigmaE = sigmaE, 
    Pfall = Pfall,
    Rgaps = Rgaps,
    Pdeath = Pdeath, 
    Ns = Ns, 
    Rdispersal = Rdispersal, 
    determinist =  determinist
  )
  coords <- data.frame(
    individual = 1:(grid*grid),
    X = rep(1:grid, each = grid),
    Y = rep(1:grid, grid)
  )
  lapply(list("topography" = sim$Etopo, 
              "gaps" = sim$Egaps, 
              "genotype" = sim$A, 
              "phenotype" = sim$Z), 
         function(M)
    melt(M) %>% 
    rename(generation = Var1, individual = Var2)) %>% 
    bind_rows(.id = "var") %>% 
    left_join(coords, by = "individual")
}
  