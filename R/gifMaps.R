#' @include utils-pipe.R
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr filter
#' @importFrom gganimate transition_time
NULL

#' gifMaps
#'
#' @param simulation df.  result from simulator function
#' @param variable char.  variable to visualize "topography", "gaps",
#'   "genotype", or "phenotype"
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
gifMaps <- function(simulation, variable = "genotype"){
  # add a test for variable
  X <- Y <- value <- generation <- individual <- var <- NULL
  filter(simulation, var == variable) %>% 
    ggplot(aes(X, Y, fill = value)) +
    geom_tile() +
    facet_wrap(~ var) +
    viridis::scale_fill_viridis() +
    transition_time(generation) +
    labs(title = 'Generation: {frame_time}')
}
