#' @include utils-pipe.R
#' @importFrom dplyr filter select mutate
#' @importFrom reshape2 dcast
#' @importFrom utils data
NULL

#'  Topography from Paracou
#'
#' @param grid int. Matrix grid size.
#' @param plot int. Plot number between 1 and 15 (a cell size is 3x3m)
#' @param Elim double. Environmental matrix extrme (absolute value)
#'
#' @return A matrix of TWI of ize 98x98 for the simulator.
#' 
#' @export
#'
#' @examples
#' 
#' paracouTopography(1)
#' 
paracouTopography <- function(grid = 20, plot = 1, Elim = 1){
  X <- value <- Plot <- NULL
  M <- filter(wetness, Plot == plot) %>% 
    mutate(value = Elim*scale(value)) %>% 
    dcast(X ~ Y, value = "value") %>% 
    select(-X) %>% 
    as.matrix()
  M <- M[1:min(dim(M)),1:min(dim(M))]
  if(grid > ncol(M))
    stop(paste0("Using topography fom Paracou plot ", plot, ", the grid can't excess ", ncol(M)))
  M[1:grid, 1:grid]
}

# library(tidyverse)
# wetness <- read_tsv("../PhD/documents/modelling/modelling_save/wetness.tsv")
# wetness <- wetness %>% 
#   group_by(Plot) %>% 
#   mutate(X = (X - min(X))/3, Y = (Y - min(Y))/3) %>% 
#   mutate(X = floor(X), Y = floor(Y)) %>% 
#   group_by(Plot, X, Y) %>% 
#   summarise(value = mean(value)) %>% 
#   ungroup() 
# 
# ggplot(wetness, aes(X, Y, fill = value)) +
#   geom_tile() +
#   viridis::scale_fill_viridis() +
#   facet_wrap(~ Plot)
# 
# usethis::use_data(wetness, internal = TRUE)
