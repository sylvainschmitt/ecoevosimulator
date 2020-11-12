#' @include sinusoidalTopography.R squareDiamondTopography.R paracouTopography.R
NULL

#' Generate topography
#'
#' @param grid int.  Number of cells per side of the matrix.
#' @param topography char. Topography generator between 'sinusoidal',
#'   'squarediamond' or 'paracou'.
#' @param Elim double. Environmental matrix extrme (absolute value).
#' @param amplitude double. Amplitude of the sinusoidal functional.
#' @param ruggedness double. Ruggedness parameter.
#' @param plot int. Plot number between 1 and 15 (a cell size is 3x3m).
#'
#' @return A matrix of TWI for the simulator.
#' @export
#'
#' @examples
#' generateTopography()
#' 
generateTopography <- function(
  grid = 20,
  topography = "sinusoidal",
  Elim = 5,
  amplitude = 1,
  ruggedness = 1,
  plot = 1
){
  if(!(topography %in% c('sinusoidal', 'squarediamond', 'paracou')))
    stop(paste(topography, "option doesn't exist to generate topography (see help)."))
  if(topography == 'sinusoidal')
    Topo <- sinusoidalTopography(grid = grid, Elim = Elim, amplitude = amplitude)
  if(topography == 'squarediamond')
    Topo <- squareDiamondTopography(grid = grid,  ruggedness = ruggedness)
  if(topography == 'paracou')
    Topo <- paracouTopography(grid = grid, plot = plot, Elim = Elim)
  return(Topo)
}