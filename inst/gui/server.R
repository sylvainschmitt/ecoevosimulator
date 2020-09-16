library(ecoevosimulator)
library(shiny)
library(shinydashboard)
theme_set(bayesplot::theme_default())

server <- function(input, output) {
  parameters <- reactiveValues(grid = 20,
                               Ngen = 50,
                               muG = 0,
                               sigmaG = 1,
                               muE = 0,
                               sigmaE = 1,
                               Elim = 5,
                               seedlings = 4,
                               dispersal = 1,
                               viability_deterministic = TRUE)
  
  observeEvent(input$simulate, {
    parameters$grid <- input$grid
    parameters$Ngen <- input$Ngen
    parameters$muG <- input$muG
    parameters$sigmaG <- input$sigmaG
    parameters$muE <- input$muE
    parameters$sigmaE <- input$sigmaE
    parameters$Elim <- input$Elim
    parameters$seedlings <- input$seedlings
    parameters$dispersal <- input$dispersal
    parameters$viability_deterministic <- input$viability_deterministic
  })
  
  observeEvent(input$quit, {
    stopApp()
  })
 
  output$simulator = renderPlot(height = 700,
                                plotSim(simulator(grid = parameters$grid,
                                                  Ngen = parameters$Ngen,
                                                  muG = parameters$muG,
                                                  sigmaG = parameters$sigmaG,
                                                  muE = parameters$muE,
                                                  sigmaE = parameters$sigmaE,
                                                  Elim = parameters$Elim,
                                                  seedlings = parameters$seedlings,
                                                  dispersal = parameters$dispersal,
                                                  viability_deterministic = parameters$viability_deterministic)))
}
