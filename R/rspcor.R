#' @include utils-pipe.R
#' @importFrom raster as.matrix aggregate disaggregate raster
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate n
#' 
NULL

#' Spatially auto-correlated distribution
#'
#' Spatial random generator of spatially auto-correlated distribution based on a
#' given random generator. The generator uses a simple spatial resampling of a
#' smaller grid.
#'
#' @param M matrix. Environmental matrix in which to create spatial
#'   auto-correlation.
#' @param dcor int. Spatial auto-correlation size in number of cells.
#'
#' @return A data.frame of X, Y, and values.
#' @export
#'
#' @examples
#' rspcor()
#' 
rspcor <- function(
  M =  matrix(rdeltanci(NCI = rnorm(50*50, mean = 122.73, sd = 25.806)), 
              ncol = 50, nrow = 50),
  dcor = 3 # spatial auto-correlation size (3*3m)
){
  Var1 <- Var2 <- NULL
  M <- raster(M)
  M <- aggregate(M, dcor)
  M <- disaggregate(M, dcor, method = "bilinear")
  as.matrix(M)[1:50,1:50] %>% 
    melt() %>% 
    rename(X = Var1, Y = Var2)
} 
