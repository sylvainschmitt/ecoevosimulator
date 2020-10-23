#' @include utils-pipe.R generateNCI.R RcppExports.R sinusoidalTopography.R
#'   squareDiamondTopography.R paracouTopography.R
#' @importFrom dplyr bind_rows left_join rename
#' @importFrom reshape2 melt
#' @importFrom tidyr separate
NULL

#' simulator
#'
#' @param grid int.  Number of cells per side of the matrix.
#' @param Nt int. Number of time steps.
#' @param topography char. Topography generator between 'sinusoidal',
#'   'squarediamond' or 'paracou'.
#' @param Elim double. Environmental matrix extrme (absolute value).
#' @param amplitude double. Amplitude of the sinusoidal functional.
#' @param rudgeness double. Rugedness parameter.
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
#' @param plot int. Plot number between 1 and 15 (a cell size is 3x3m).
#' @param sigmaGtopo double. Variance of genetic values with topography.
#' @param sigmaZtopo double. Plasticity of phenotypes with topography.
#' @param sigmaGnci double. Variance of genetic values with NCI.
#' @param sigmaZnci double. Plasticity of phenotypes with NCI.
#' @param Pdeath double. Background mortality probability.
#' @param Ns int. Number of seedlings per cell.
#' @param Rdispersal int. Dispersal radius in cells.
#' @param determinist bool. Deterministic or probabilistic vaibility.
#' @param verbose bool. Should the function print statuses.
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
  muNCI = 124, # mu of normal distributon for nci
  sigmaNCI = 26, # sigma of normal distributon nci
  p = 0.271, # probability to be positive
  mu = 0.749, # mu of lognormal distributon for positive deltanci
  sigma = 2.651, # sigma of lognormal distributon for positive deltanci
  lambda = 0.31, # lamnda of exponential distributon for negative deltanci
  d = 3, # spatial autocorrelation size (3*3m)
  sigmaGtopo = 1,
  sigmaZtopo = 1,
  sigmaGnci = 26,
  sigmaZnci = 26,
  Pdeath = 0.01325548,
  Ns = 4,
  Rdispersal = 1,
  determinist = TRUE,
  verbose = TRUE
){
  Var1 <- Var2 <- var <- NULL
  
  # Topo
  if(verbose) message("Generating topography.")
  if(!(topography %in% c('sinusoidal', 'squarediamond', 'paracou')))
    stop(paste(topography, "option doesn't exist to generate topography (see help)."))
  if(topography == 'sinusoidal')
    Topo <- sinusoidalTopography(grid = grid, Elim = Elim, amplitude = amplitude)
  if(topography == 'squarediamond')
    Topo <- squareDiamondTopography(grid = grid,  rudgeness = rudgeness)
  if(topography == 'paracou')
    Topo <- paracouTopography(grid = grid, plot = plot, Elim = Elim)
  
  # NCI
  if(verbose) message("Generating NCI.")
  NCI <- generateNCIsim(
    grid = grid, 
    Nt = Nt, 
    muNCI = muNCI, 
    sigmaNCI = sigmaNCI, 
    p = p, 
    mu = mu,
    sigma = sigma,
    lambda = lambda,
    d = d)
  
  # Sim
  if(verbose) message("Running simulation.")
  sim <- simulatorCpp(
    Topo = Topo,
    NCI = NCI,
    grid = grid, 
    Nt = Nt, 
    sigmaGtopo = sigmaGtopo,
    sigmaZtopo = sigmaZtopo,
    sigmaGnci = sigmaGnci,
    sigmaZnci = sigmaZnci,
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
  