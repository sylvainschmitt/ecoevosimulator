library(ecoevosimulator)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gganimate)
library(bayesplot)
theme_set(bayesplot::theme_default())

server <- function(input, output) {
  parameters <- reactiveValues(
    grid = 20,
    Nt = 50,
    timestep = 30,
    topography = "sinusoidal",
    Elim = 5,
    amplitude = 1,
    ruggedness = 1,
    plot = 1,
    muNCI = 124, 
    sigmaNCI = 26,
    alpha = -1.32,
    beta = 0.003,
    mu = 0.749,
    sigma = 2.651,
    lambda = 0.31,
    d = 3,
    sigmaGtopo = 1,
    sigmaZtopo = 1,
    sigmaGnci = 26,
    sigmaZnci = 26,
    Pdeath = 1,
    Ns = 4,
    Rpollination = 1,
    Rdispersion = 1,
    determinist = TRUE,
    verbose = TRUE
  )
  
  gif <- reactiveValues(calculate = FALSE)
  
  observeEvent(input$simulate, {
    parameters$grid <- input$grid
    parameters$Nt <- input$Nt
    parameters$timestep <- input$timestep
    parameters$topography <- input$topography
    parameters$Elim <- input$Elim
    parameters$amplitude <- input$amplitude
    parameters$ruggedness <- input$ruggedness
    parameters$plot <- input$plot
    parameters$muNCI <- input$muNCI
    parameters$sigmaNCI <- input$sigmaNCI
    parameters$alpha <- input$alpha
    parameters$beta <- input$beta
    parameters$mu <- input$mu
    parameters$sigma <- input$sigma
    parameters$lambda <- input$lambda
    parameters$d <- input$d
    parameters$sigmaGtopo <- input$sigmaGtopo
    parameters$sigmaZtopo <- input$sigmaZtopo
    parameters$sigmaGnci <- input$sigmaGnci
    parameters$sigmaZnci <- input$sigmaZnci
    parameters$Pdeath <- input$Pdeath
    parameters$Ns <- input$Ns
    parameters$Rpollination <- input$Rpollination
    parameters$Rdispersion <- input$Rdispersion
    parameters$determinist <- input$determinist
    gif$calculate <- FALSE
  })
  
  observeEvent(input$compgif, {
    gif$calculate <- TRUE
  })
  
  observeEvent(input$quit, {
    stopApp()
  })
  
  output$simulator = renderPlot(plotSim(simulator(
    grid = parameters$grid,
    Nt = parameters$Nt,
    timestep = parameters$timestep,
    topography = parameters$topography,
    Elim = parameters$Elim,
    amplitude = parameters$amplitude,
    ruggedness = parameters$ruggedness,
    plot = parameters$plot,
    muNCI = parameters$muNCI, 
    sigmaNCI = parameters$sigmaNCI,
    alpha = parameters$alpha,
    beta = parameters$beta,
    mu = parameters$mu,
    sigma = parameters$sigma,
    lambda = parameters$lambda,
    d = parameters$d,
    sigmaGtopo = parameters$sigmaGtopo,
    sigmaZtopo = parameters$sigmaZtopo,
    sigmaGnci = parameters$sigmaGnci,
    sigmaZnci = parameters$sigmaZnci,
    Pdeath = parameters$Pdeath,
    Ns = parameters$Ns,
    Rpollination = parameters$Rpollination,
    Rdispersion = parameters$Rdispersion,
    determinist = parameters$determinist,
    verbose = TRUE
    )))
  
  output$gif = renderImage({
    outfile <- tempfile(fileext='.gif')
    if(gif$calculate){
      p <- gifMaps(simulator(
        grid = parameters$grid,
        Nt = parameters$Nt,
        timestep = parameters$timestep,
        topography = parameters$topography,
        Elim = parameters$Elim,
        amplitude = parameters$amplitude,
        ruggedness = parameters$ruggedness,
        plot = parameters$plot,
        muNCI = parameters$muNCI, 
        sigmaNCI = parameters$sigmaNCI,
        alpha = parameters$alpha,
        beta = parameters$beta,
        mu = parameters$mu,
        sigma = parameters$sigma,
        lambda = parameters$lambda,
        d = parameters$d,
        sigmaGtopo = parameters$sigmaGtopo,
        sigmaZtopo = parameters$sigmaZtopo,
        sigmaGnci = parameters$sigmaGnci,
        sigmaZnci = parameters$sigmaZnci,
        Pdeath = parameters$Pdeath,
        Ns = parameters$Ns,
        Rpollination = parameters$Rpollination,
        Rdispersion = parameters$Rdispersion,
        determinist = parameters$determinist,
        verbose = TRUE
      ))
      anim_save("outfile.gif", animate(p))

    } else{
      p <- ggplot(data.frame(t = 1:2)) + 
        theme_void() +
        transition_time(t)
      anim_save("outfile.gif", animate(p, 2, 2))
    }
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 800,
         height = 800)
  }, deleteFile = TRUE)
}
