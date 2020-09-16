#' @include utils-pipe.R
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr filter
#' @importFrom reshape2 melt dcast
#' @importFrom gridExtra grid.arrange
NULL

#' plotSim
#'
#' @param simulation df.  result from simulator function
#' @param gen int.  generation to be plotted
#' 
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotSim(simulator())
#' 
plotSim <- function(simulation, gen = 1){
  gridExtra::grid.arrange(
    plotEnv(simulation, gen),
    plotMaps(simulation),
    plotTrajectories(simulation),
    nrow = 2,
    widths = c(1,2),
    layout_matrix = rbind(c(1, 2),
                          c(3, 3))
  )
}
  
#' plotEnv
#'
#' @param simulation df.  result from simulator function
#' @param gen int.  generation to be plotted
#'
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotEnv(simulator())
#' 
plotEnv <- function(simulation, gen = 1){
  X <- Y <- value <- generation <- individual <- var <- NULL
  filter(simulation, 
         var == "ecotype", 
         generation == gen) %>%
    ggplot(aes(X, Y, fill = value)) +
    geom_tile() +
    viridis::scale_fill_viridis(guide = "none")
}

#' plotMaps
#'
#' @param simulation df.  result from simulator function
#'
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotMaps(simulator())
#' 
plotMaps <- function(simulation){
  X <- Y <- value <- generation <- individual <- var <- NULL
  filter(simulation, 
         var != "ecotype",
         generation %in% c(1, floor(max(simulation$generation)/2), max(simulation$generation))) %>% 
    ggplot(aes(X, Y, fill = value)) +
    facet_wrap(~ generation) +
    geom_tile() +
    facet_grid(var ~ generation) +
    viridis::scale_fill_viridis(guide = "none")
}

#' plotTrajectories
#'
#' @param simulation df.  result from simulator function
#'
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotTrajectories(simulator())
#' 
plotTrajectories <- function(simulation){
  X <- Y <- value <- generation <- individual <- ecotype <- NULL
  dcast(simulation, generation + individual + X + Y ~ var) %>%
    melt(id.vars = c("generation", "individual", "X", "Y", "ecotype")) %>% 
    ggplot(aes(generation, value, 
               group = individual, col = ecotype)) + 
    geom_line(alpha = 0.5) +
    facet_wrap(~ variable) +
    viridis::scale_color_viridis(guide = "none")
}


