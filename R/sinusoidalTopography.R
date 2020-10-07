#' Topography generator with the sinusoidal function
#' 
#' E = (-Elim, Elim)
#' M = sin(amplitude.E*t(E))
#' Have a look to the topography vignette.
#'
#' @param grid int. Matrix grid size should be of size 2^n+1
#' @param Elim double. Environmental matrix extrme (absolute value)
#' @param amplitude double. Amplitude of the sinusoidal functional
#'
#' @return
#' @export
#'
#' @examples
#' sinusoidalTopography(grid = 5, Elim = 10, amplitude = 0.05)
#' 
sinusoidalTopography <- function(
  grid = 2^6+1, # must be of size 2^n+1
  Elim = 10,
  amplitude = 0.1
){
  E <- seq(from = -Elim, to = Elim, length.out = grid)
  M <- E %*% t(E)
  M <- sin(amplitude*M)
  return(M)
}
