library(ecoevosimulator)
library(shiny)
library(shinydashboard)
theme_set(bayesplot::theme_default())

ui <- dashboardPage(
    dashboardHeader(title = "Eco-evo simulator 1D"),
    dashboardSidebar(
        tags$head(tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"))), 
        actionButton("simulate", "Simulate", icon = icon("recycle"), width = "200px"),
        actionButton("quit", "Quit", icon = icon("sign-out-alt"), width = "200px"),
        checkboxInput("viability_deterministic", "Determinist/Probabilist viability", value = TRUE),
        sliderInput("grid", "Number of cells per side:",
                    min = 1, max = 100, value = 20),
        sliderInput("Ngen", "Number of generations:",
                    min = 1, max = 500, value = 50),
        sliderInput("muG", "Genetic mean:",
                    min = -10, max = 10, value = 0),
        sliderInput("sigmaG", "Genetic variance:",
                    min = 0, max = 10, value = 1),
        sliderInput("muE", "Environmental mean:",
                    min = -10, max = 10, value = 0),
        sliderInput("sigmaE", "Environmental variance:",
                    min = 0, max = 10, value = 1),
        sliderInput("Elim", "Environmental gradient size:",
                    min = 0, max = 100, value = 5),
        sliderInput("seedlings", "Seedlings:",
                    min = 0, max = 50, value = 4),
        sliderInput("dispersal", "Dispersal:",
                    min = 0, max = 50, value = 1)
    ),
    dashboardBody(fluidRow(plotOutput("simulator")))
)