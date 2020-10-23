#' @include utils-pipe.R simulator.R
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr filter select group_by ungroup
#' @importFrom reshape2 melt dcast
#' @importFrom gridExtra grid.arrange
NULL

#' plotSim
#'
#' @param simulation df.  result from simulator function
#' @param time int.  generation to be plotted
#' 
#' @return A ggplot.
#' 
#' @export
#'
#' @examples
#' 
#' plotSim(simulator())
#' 
plotSim <- function(simulation, time = 1){
  grid.arrange(
    plotMaps(simulation),
    plotTrajectories(simulation),
    ncol = 2,
    widths = c(1,1),
    layout_matrix = rbind(c(1, 2))
  )
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
  X <- Y <- value <- timestep <- individual <- var <- variable <- NULL
  filter(simulation, 
         timestep %in% c(1, floor(max(simulation$timestep)/2), max(simulation$timestep))) %>% 
    # group_by(variable, type) %>% 
    group_by(variable) %>% 
    mutate(value = scale(value)) %>% 
    ggplot(aes(X, Y, fill = value)) +
    facet_wrap(~ timestep) +
    geom_tile() +
    facet_grid(variable + type ~ timestep) +
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
  X <- Y <- value <- timestep <- individual <- variable <- env <- NULL
  dcast(simulation, timestep + individual + X + Y + variable ~ type) %>%
    mutate(env = environment) %>% 
    melt(id.vars = c("timestep", "individual", "X", "Y", "variable", "env"),
         variable.name = "type") %>%
    group_by(variable) %>%
    mutate(env = scale(env)) %>%
    ungroup() %>% 
    ggplot(aes(timestep, value,
               group = individual, col = env)) +
    geom_line(alpha = 0.5) +
    facet_grid(variable ~ type, scales = "free") +
    viridis::scale_color_viridis(guide = "none")
}
