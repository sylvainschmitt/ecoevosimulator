#' @include utils-pipe.R
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr filter
#' @importFrom gganimate transition_time
NULL

#' gifMaps
#'
#' @param simulation df. Result from simulator function.
#' @param type char. Type to visualize "variable", "genotype", or
#'   "phenotype".
#' @param variable char. Variable to visualize "topography" or "nci".
#'
#' @return A ggplot.
#'
#' @export
#'
#' @examples
#'
#' simulation <- simulator(grid = 50)
#' gifMaps(simulator())
#' 
gifMaps <- function(simulation, type = "genotype", variable = "topography"){
  # add a test for variable
  X <- Y <- value <- individual <- var <- timestep <- NULL
  filter(simulation, type == {{type}}, variable == {{variable}}) %>% 
    ggplot(aes(X, Y, fill = value)) +
    geom_tile() +
    facet_wrap(~ type + variable) +
    viridis::scale_fill_viridis() +
    transition_time(timestep) +
    labs(title = 'Timestep: {frame_time}')
}
