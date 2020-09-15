#' @import ggplot2
#' @importFrom viridis scale_color_viridis
NULL

#' Title
#'
#' @param simulation 
#'
#' @return
#' @export
#'
#' @examples
plotSim <- function(simulation){
  ggplot(simulation, aes(generation, value, 
                         group = individual, col = environment)) + 
    geom_line(alpha = 0.5) +
    facet_wrap(~ var) +
    scale_color_viridis()
}
  