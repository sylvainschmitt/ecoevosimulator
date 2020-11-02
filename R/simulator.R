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
#' @param timestep int. Time-step length in years.
#' @param topography char. Topography generator between 'sinusoidal',
#'   'squarediamond' or 'paracou'.
#' @param Elim double. Environmental matrix extrme (absolute value).
#' @param amplitude double. Amplitude of the sinusoidal functional.
#' @param ruggedness double. Ruggedness parameter.
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
#' @param plot int. Plot number between 1 and 15 (a cell size is 3x3m).
#' @param sigmaGtopo double. Variance of genetic values with topography.
#' @param sigmaZtopo double. Plasticity of phenotypes with topography.
#' @param sigmaGnci double. Variance of genetic values with NCI.
#' @param sigmaZnci double. Plasticity of phenotypes with NCI.
#' @param Pdeath double. Background mortality probability.
#' @param Ns int. Number of seedlings per cell.
#' @param Rpollination int. Pollination radius in cells (father to mother).
#' @param Rdispersion int. Dispersal radius in cells (mother to seedling).
#' @param determinist bool. Deterministic or probabilistic viability.
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
  timestep = 30, # time-step length in years
  topography = "sinusoidal",
  Elim = 5,
  amplitude = 1,
  ruggedness = 1,
  plot = 1,
  muNCI = 124, # mu of normal distribution for NCI
  sigmaNCI = 26, # sigma of normal distribution NCI
  alpha = -1.32,
  beta = 0.003,
  mu = 0.749, # mu of lognormal distribution for positive deltaNCI
  sigma = 2.651, # sigma of lognormal distribution for positive deltaNCI
  lambda = 0.31, # lambda of exponential distribution for negative deltaNCI
  d = 3, # spatial auto-correlation size (3*3m)
  sigmaGtopo = 1,
  sigmaZtopo = 1,
  sigmaGnci = 26,
  sigmaZnci = 26,
  Pdeath = 0.01325548,
  Ns = 4,
  Rpollination = 1,
  Rdispersion = 1,
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
    Topo <- squareDiamondTopography(grid = grid,  ruggedness = ruggedness)
  if(topography == 'paracou')
    Topo <- paracouTopography(grid = grid, plot = plot, Elim = Elim)
  
  # NCI
  if(verbose) message("Generating NCI.")
  NCI <- generateNCIsim(
    grid = grid, 
    Nt = Nt, 
    timestep = timestep,
    muNCI = muNCI, 
    sigmaNCI = sigmaNCI, 
    alpha = alpha,
    beta = beta,
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
  