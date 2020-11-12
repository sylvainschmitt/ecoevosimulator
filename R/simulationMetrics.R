#' @include utils-pipe.R simulator.R
#' @importFrom stats var kmeans sd
#' @importFrom dplyr filter mutate recode select group_by summarise bind_rows summarise_all
#' @importFrom reshape2 dcast melt
NULL

#' Compute simulation metrics
#'
#' @param simulation df.  result from simulator function
#' @param thin int. thinning results by thin timesteps
#'
#' @return A data frame with Dg and WSSscore
#'
#' @export
#'
#' @examples
#' simulation <- simulatorAll(grid = 10)
#' metrics <- simulationMetrics(simulation)
#' 
simulationMetrics <- function(simulation,
                               thin = 1){
  
  variable <- X <- Y <- type <- e_topo <- g_topo <- e_nci <- g_nci <- timestep <- Dg_topo <- Dg_nci <- topo <- nci <- NULL
  
  dg <- simulation %>%
    mutate(variable = recode(variable, "topography" = "topo")) %>%
    select(-X, -Y) %>% 
    mutate(variable = paste0(substr(type, 1, 1), "_", variable)) %>%
    dplyr::select(-type) %>%
    reshape2::dcast(timestep + individual ~ variable, value.var = "value") %>%
    mutate(
      Dg_topo = sqrt(((e_topo - g_topo)/sd(e_topo))^2),
      Dg_nci = sqrt(((e_nci - g_nci)/sd(e_nci))^2)
    ) %>%
    select(timestep, Dg_topo, Dg_nci) %>%
    group_by(timestep) %>%
    summarise_all(mean) %>%
    reshape2::melt("timestep") %>%
    mutate(variable = gsub("Dg_", "", variable)) %>%
    mutate(metric = "Dg")
  wss <- simulation %>%
    filter(type == "genotype") %>%
    mutate(variable = recode(variable, "topography" = "topo")) %>%
    dplyr::select(-X, -Y, -type) %>% 
    reshape2::dcast(timestep + individual ~ variable) %>%
    group_by(timestep) %>%
    summarise(wss_score_topo = .wss_score(topo),
              wss_score_nci = .wss_score(nci)) %>%
    melt("timestep") %>%
    mutate(variable = gsub("wss_score_", "", variable)) %>%
    mutate(metric = "wss score")
  
  return(bind_rows(dg, wss) %>% 
           filter(timestep %in% round(seq(from = min(simulation$timestep), 
                                          to = max(simulation$timestep), 
                                          length.out = length(unique(simulation$timestep))/thin))))
}

.wss_score <- function(var1, var2 = NULL, k = 2) {
  if(!is.null(var2))
    df <- data.frame(var1 = var1, var2 = var2)
  else
    df <- data.frame(var1 = var1)
  x <- (nrow(df)-1)*sum(apply(df,2,var))
  wss1 <- sum(kmeans(df, centers = 1)$withinss)
  wssk <- sum(kmeans(df, centers = k)$withinss)
  (wss1 / wssk) / k 
}  

