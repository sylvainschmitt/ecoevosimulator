library(ecoevosimulator)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gganimate)
library(bayesplot)
theme_set(bayesplot::theme_default())

server <- function(input, output) {
  parameters <- reactiveValues(grid = 20,
                               Nt = 50,
                               Elim = 5,
                               muG = 0,
                               sigmaG = 1,
                               muE = 0,
                               sigmaE = 1,
                               Pfall = 0.01,
                               Rgaps = 2,
                               Pdeath = 0.1,
                               Ns = 4,
                               Rdispersal = 1,
                               determinist = TRUE)
  
  gif <- reactiveValues(calculate = FALSE)
  
  observeEvent(input$simulate, {
    parameters$grid <- input$grid
    parameters$Nt <- input$Nt
    parameters$Elim <- input$Elim
    parameters$sigmaG <- input$sigmaG
    parameters$sigmaE <- input$sigmaE
    parameters$Pfall <- input$Pfall
    parameters$Rgaps <- input$Rgaps
    parameters$Pdeath <- input$Pdeath
    parameters$Ns <- input$Ns
    parameters$Rdispersal <- input$Rdispersal
    parameters$determinist <- input$determinist
    gif$calculate <- FALSE
  })
  
  observeEvent(input$compgif, {
    gif$calculate <- TRUE
  })
  
  observeEvent(input$quit, {
    stopApp()
  })
  
  output$simulator = renderPlot(plotSim(simulator(grid = parameters$grid,
                        Nt = parameters$Nt,
                        Elim = parameters$Elim,
                        muG = 0,
                        sigmaG = parameters$sigmaG,
                        muE = 0,
                        sigmaE = parameters$sigmaE,
                        Pfall = parameters$Pfall,
                        Rgaps = parameters$Rgaps,
                        Pdeath = parameters$Pdeath,
                        Ns = parameters$Ns,
                        Rdispersal = parameters$Rdispersal,
                        determinist = parameters$determinist)))
  
  output$gif = renderImage({
    outfile <- tempfile(fileext='.gif')
    if(gif$calculate){
      p <- gifMaps(simulator(grid = parameters$grid,
                          Nt = parameters$Nt,
                          Elim = parameters$Elim,
                          muG = 0,
                          sigmaG = parameters$sigmaG,
                          muE = 0,
                          sigmaE = parameters$sigmaE,
                          Pfall = parameters$Pfall,
                          Rgaps = parameters$Rgaps,
                          Pdeath = parameters$Pdeath,
                          Ns = parameters$Ns,
                          Rdispersal = parameters$Rdispersal,
                          determinist = parameters$determinist))
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
