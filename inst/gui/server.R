library(ecoevosimulator)
library(shiny)
library(shinydashboard)
# theme_set(bayesplot::theme_default())

server <- function(input, output) {
    observeEvent(input$simulate, {
        output$simulator = renderPlot(height = 600,
                                      plotSim(simulator(Nind = input$Nind,
                                                                Ngen = input$Ngen,
                                                                muG = input$muG,
                                                                sigmaG = input$sigmaG,
                                                                muE = input$muE,
                                                                sigmaE = input$sigmaE,
                                                                Elim = input$gradientlim,
                                                                seedlings = input$seedlings,
                                                                dispersal = input$dispersal,
                                                                viability_deterministic = input$viability_deterministic))) 
    })
}
