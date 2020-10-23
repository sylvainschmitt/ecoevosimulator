#' Topography generator with the sinusoidal function
#' 
#' E = (-Elim, Elim)
#' M = sin(amplitude.E*t(E))
#' Have a look to the topography vignette.
#'
#' @param grid int. Matrix grid size.
#' @param Elim double. Environmental matrix extrme (absolute value).
#' @param amplitude double. Amplitude of the sinusoidal functional.
#'
#' @return Environmental matrix of size grid x grid for the simulator.
#' 
#' @export
#'
#' @examples
#' sinusoidalTopography(grid = 5, Elim = 10, amplitude = 0.05)
#' 
sinusoidalTopography <- function(
  grid = 20,
  Elim = 10,
  amplitude = 1
){
  E <- seq(from = -1, to = 1, length.out = grid)
  M <- E %*% t(E)
  M <- Elim*sin(amplitude*M)
  return(M)
}
