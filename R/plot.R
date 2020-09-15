#' @import ggplot2
#' @importFrom viridis scale_color_viridis
NULL

#' plotSim
#'
#' @param simulation df.  result from simulator function
#'
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotSim(simulator())
#' 
plotSim <- function(simulation){
  ggplot(simulation, aes(generation, value, 
                         group = individual, col = environment)) + 
    geom_line(alpha = 0.5) +
    facet_wrap(~ var) +
    scale_color_viridis()
}
  