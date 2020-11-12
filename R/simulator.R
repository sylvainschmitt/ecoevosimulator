#' @include utils-pipe.R generateNCI.R RcppExports.R generateTopography.R
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom reshape2 melt
#' @importFrom tidyr separate
NULL

#' Simulator
#'
#' @param grid int.  Number of cells per side of the matrix.
#' @param Nt int. Number of time steps.
#' @param timestep int. Time-step length in years.
#' @param topography matrix. Topography matrix generated with generateTopography.
#' @param NCI matrix. Topography matrix generated with generateNCIsim.
#' @param sigmaGtopo double. Variance of genetic values with topography.
#' @param sigmaZtopo double. Plasticity of phenotypes with topography.
#' @param sigmaGnci double. Variance of genetic values with NCI.
#' @param sigmaZnci double. Plasticity of phenotypes with NCI.
#' @param Pdeath double. Background mortality probability.
#' @param Ns int. Number of seedlings per cell.
#' @param Rpollination int. Pollination radius in cells (father to mother).
#' @param Rdispersion int. Dispersal radius in cells (mother to seedling).
#' @param determinist bool. Deterministic or probabilistic viability.
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
  timestep = 30, # time-step length in years
  topography = generateTopography(),
  NCI = generateNCI(),
  sigmaGtopo = 1,
  sigmaZtopo = 1,
  sigmaGnci = 26,
  sigmaZnci = 26,
  Pdeath = 0.01325548,
  Ns = 4,
  Rpollination = 1,
  Rdispersion = 1,
  determinist = TRUE
){
  Var1 <- Var2 <- var <- NULL
  if(!all(dim(topography) == grid))
    stop(paste("Topography is not of size grid =", grid, "but of size", dim(topography)[1], "x", dim(topography)[2]))
  if(ncol(NCI) != grid*grid)
    stop(paste("Columns of NCI are not of size grid*grid =", grid*grid, "but of size", ncol(NCI)))
  if(nrow(NCI) != Nt)
    stop(paste("Rows of NCI are not of size Nt =", Nt, "but of size", nrow(NCI)))
  sim <- simulatorCpp(
    Topo = topography,
    NCI = NCI,
    grid = grid, 
    Nt = Nt, 
    timestep = timestep,
    sigmaGtopo = sigmaGtopo,
    sigmaZtopo = sigmaZtopo,
    sigmaGnci = sigmaGnci,
    sigmaZnci = sigmaZnci,
    Pdeath = Pdeath, 
    Ns = Ns, 
    Rpollination = Rpollination,
    Rdispersion = Rdispersion, 
    determinist =  determinist
  )
  coords <- data.frame(
    individual = 1:(grid*grid),
    X = rep(1:grid, each = grid),
    Y = rep(1:grid, grid)
  )
  lapply(list(
    "environment_topography" = sim$Topography,
    "genotype_topography" = sim$Atopo, 
    "phenotype_topography" = sim$Ztopo,
    "environment_nci" = sim$NCI,
    "genotype_nci" = sim$Anci, 
    "phenotype_nci" = sim$Znci
  ), 
  function(M)
    melt(M) %>% 
    rename(timestep = Var1, individual = Var2)) %>% 
    bind_rows(.id = "var") %>% 
    left_join(coords, by = "individual") %>% 
    separate(var, c("type", "variable"), sep = "_")
}
  