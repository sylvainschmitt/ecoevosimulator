#' @include utils-pipe.R
#' @importFrom raster as.matrix disaggregate raster
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate n
#' 
NULL

#' Spatialy autocorrelated distribution
#'
#' Spatial random generator of spatially autocorrelated distribution based on a
#' given random generator. The generator uses a simple spatial resamplng of a
#' smaler grid.
#'
#' @param grid int. Matrix grid size.
#' @param generator char. Name of the random generator function (e.g.
#'   rdeltanci).
#' @param args list. List of arguments used in the random generator at the
#'   exception of the number of observations n.
#' @param dcor int. Spatial autoccorelation size in number of cells.
#'
#' @return A data.frame of X, Y, and values.
#' @export
#'
#' @examples
#' rspcor(50)
#' 
rspcor <- function(
  grid = 50, # grid size
  generator = "rdeltanci", # can be rnorm for nci
  args = list(p = 0.271, mu = 0.749, sigma = 2.651, lambda = 0.31), # arguments for the random generator
  dcor = 3 # spatial autocorrelation size (3*3m)
){
  Var1 <- Var2 <- NULL
  grid2 <- ceiling(grid/dcor)
  args["n"] <- grid2^2
  M2 <- matrix(do.call(generator, as.list(args)), 
               ncol = grid2, nrow = grid2)
  as.matrix(disaggregate(raster(M2), dcor, method="bilinear"))[1:grid, 1:grid] %>% 
    melt() %>% 
    rename(X = Var1, Y = Var2) %>% 
    mutate(Ind = 1:n())
} 
